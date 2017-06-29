#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>

#include <arpa/inet.h>
#include <signal.h>
#include <sys/mman.h>
#include <ucontext.h>

/*
  This program is designed to be a remote runner of programs with test vectors.  It:

  1) reads a test vector and test program off of stdin,
  2) maps the test program as executable (splicing in code at the end to raise a SIGTRAP),
  3) catches the SIGTRAP and takes a snapshot of the register and memory state, and
  4) sends the result out over stdout.


  Notes:

  * We don't deal with endianness; it is the responsibility of the test
    generator to send data in the correct endianness

  * The state of the program stack isn't tracked for test programs

  * The value of the IP after a test program isn't particularly meaningful due
    to the extra code to raise a SIGTRAP, and should be used carefully (if at all)

  * Most run-time errors are handled and reported over the channel.  A few things
    are handled via assertions.

  * Test cases are each labeled with a nonce that is written back onto the channel

  * Tests are run in-order and responses are delivered in-order.  Nonces are
    used anyway for clarity.  Responses to parse failures do not have a nonce.
    That probably isn't a big deal, though, because those failures can only
    occur if the stream ends early.

  * Test programs plus a trap instruction must fit on a single page (probably
    4k).  This isn't a major restriction considering the use case.

 */

// The number of bytes for our hand-allocated memory regions
#define MEM_REGION_BYTES 32

typedef enum {
  WORK_ITEM = 0,
  WORK_DONE = 1,
  // Possible parse errors
  WORK_ERROR_NONONCE = 2,
  WORK_ERROR_NOCONTEXT = 3,
  WORK_ERROR_NOMEM = 4,
  WORK_ERROR_NOBYTECOUNT = 5,
  WORK_ERROR_SHORT_PROGRAM = 6,
  WORK_ERROR_CTX_SIZE_MISMATCH = 7
} WorkTag;

typedef enum {
  RESPONSE_SUCCESS = 0,
  // Error reading a WorkItem (followed by a WorkTag as a uint16_t)
  RESPONSE_READ_ERROR = 1,
  // Received a SIGILL or SIGSEGV (followed by an int32_t with the signal number)
  RESPONSE_SIGNAL_ERROR = 2,
  // Failed to map the program into memory (no metadata)
  RESPONSE_ERROR_MAPFAILED = 7
} ResponseTag;

typedef enum {
  // Successful execution
  SIGNAL_SUCCESS = 0,
  // Failed with a different signal (signal number encoded separately as an int32)
  SIGNAL_OTHER = 1
} SignalTag;

// The definition of a work item
typedef struct {
  // The unique identifier of this work item.  It only needs to be unique in this process.
  uint64_t nonce;

  // The register state for the test vector
  mcontext_t ctx;

  // Another context that acts as a mask over `ctx`.  If a register value in
  // ctxMask is 1, then that register is actually modified to point to the first
  // memory buffer.  Likewise 2 and the second memory buffer.  Any other values
  // are ignored.
  //
  // This allows us to make register states also refer to memory fairly easily.
  mcontext_t ctxMask;

  // The contents of the first memory region available to the program
  uint8_t mem1[MEM_REGION_BYTES];

  // The contents of the second memory region available to the program
  uint8_t mem2[MEM_REGION_BYTES];

  // The number of bytes occupied by the test program
  uint16_t programByteCount;

  // The actual bytes of the program to be executed; note that this is
  // dynamically allocated, and must be freed with `freeWorkItem`.
  uint8_t* program;
} WorkItem;


// The context to restore after the trap is handled
ucontext_t restoreCtx;
// The context saved from the signal context
ucontext_t signalCtx;
// The type of signal received in a handler
SignalTag signalTag;
// The signal number received in the handler (probably either segv or sigill)
int32_t otherSignal;
// The size of a page in the system.  We store this as a global so we only need
// to compute it once.
int pageSize = 0;
// A pre-allocated stack, used for setting up a ucontext_t
uint8_t* programStack = NULL;


/*
  Architecture configuration
 */
#if defined(__x86_64__) || defined(__i386__)

#if defined(__x86_64__)
#define CAST_PTR(x) ((long long)(x))
#else
#define CAST_PTR(x) ((long)(x))
#endif

// Raise a trap with INT 3
uint8_t raiseTrap[] = {0xcc};

// For any register with a value of 1 or 2 in the mask, overwrite the associated
// slot in the real mcontext_t with mem1 or mem2, respectively.
//
// We have to do some ugly casts from pointers to ints here.
//
// Note: this only handles the substitution for general purpose registers.  I
// don't think it makes sense for floating point registers...
void applyMContextMask(mcontext_t* mctx, mcontext_t* mctxMask, uint8_t* mem1, uint8_t* mem2) {
  for(int i = 0; i < NGREG; ++i) {
    if(mctxMask->gregs[i] == 1) {
      mctx->gregs[i] = CAST_PTR(mem1);
    } else if(mctxMask->gregs[i] == 2) {
      mctx->gregs[i] = CAST_PTR(mem2);
    }
  }
}

#undef CAST_PTR

#elif defined(__arm__)

// This byte sequence encodes the `BKPT` instruction
uint8_t raiseTrap[] = {0xe1, 0x20, 0x00, 0x70};

// For any register with a value of 1 or 2, overwrite the associated slot with
// mem1 or mem2, respectively.
void applyMContextMask(mcontext_t* mctx, mcontext_t* mctxMask, uint8_t* mem1, uint8_t* mem2) {
  for(int i = 0; i < NGREG; ++i) {
    if(mctxMask->gregs[i] == 1) {
      mctx->gregs[i] = (int)mem1;
    } else if(mctxMask->gregs[i] == 2) {
      mctx->gregs[i] = (int)mem2;
    }
  }
}

#elif defined(__aarch64__)

uint8_t raiseTrap[] = {};
void applyMContextMask(mcontext_t* mctx, mcontext_t* mctxMask, uint8_t* mem1, uint8_t* mem2) {
  assert(0);
}

#elif defined(__powerpc__)

#if defined(__powerpc64__)
#define CAST_PTR(x) ((long long)(x))
#else
#define CAST_PTR(x) ((long)(x))
#endif

uint8_t raiseTrap[] = {};
void applyMContextMask(mcontext_t* mctx, mcontext_t* mctxMask, uint8_t* mem1, uint8_t* mem2) {
  for(int i = 0; i < NGREG; ++i) {
    if(mctxMask->gp_regs[i] == 1) {
      mctx->gp_regs[i] = CAST_PTR(mem1);
    } else if(mctxMask->gp_regs[i] == 2) {
      mctx->gp_regs[i] = CAST_PTR(mem2);
    }
  }
}

#undef CAST_PTR

#endif

// Allocate space for the program in the work item based on the
// `programByteCount` field.
//
// If the byte count is zero, this function is a no-op.
void allocateProgramSpace(WorkItem* item) {
  if(item->programByteCount == 0)
    return;

  item->program = calloc(item->programByteCount, 1);
}


/*
  Read a work item off of the stream (or report an error/end of work).

  The first thing we do is check the type of work coming at us (either a marker
  indicating that we are done, or any other value, which indicates that we are
  expecting a work item).

  The work format will be:

  1) The work item nonce (uint64_t)
  2) The size of the mcontext_t as a uint16_t (verified against the size expected in this process)
  3) A mcontext_t (that matches the definition in ucontext.h)
  4) Another mcontext_t that acts as a mask
  5) A 32 byte buffer representing the state of the first distinguished memory location
  6) A 32 byte buffer representing the state of the second distinguished memory location
  7) A uint16_t denoting the number of bytes in the test program
  8) The program to run
 */
WorkTag readWorkItem(FILE* stream, WorkItem* item) {
  uint8_t tagByte;
  size_t nItems = fread(&tagByte, 1, sizeof(uint8_t), stream);
  if(nItems == 0 || tagByte == WORK_DONE)
    return WORK_DONE;

  // Note that we don't byte swap the nonce since it doesn't really matter on
  // this end
  nItems = fread(&item->nonce, 1, sizeof(item->nonce), stream);
  if(nItems == 0) {
    return WORK_ERROR_NONONCE;
  }

  uint16_t ctxSize;
  nItems = fread(&ctxSize, 1, sizeof(ctxSize), stream);
  if(nItems == 0 || ctxSize != sizeof(item->ctx)) {
    return WORK_ERROR_CTX_SIZE_MISMATCH;
  }

  nItems = fread(&item->ctx, 1, sizeof(item->ctx), stream);
  if(nItems == 0) {
    return WORK_ERROR_NOCONTEXT;
  }

  nItems = fread(&item->ctxMask, 1, sizeof(item->ctxMask), stream);
  if(nItems == 0) {
    return WORK_ERROR_NOCONTEXT;
  }

  nItems = fread(item->mem1, 1, sizeof(item->mem1), stream);
  if(nItems == 0) {
    return WORK_ERROR_NOMEM;
  }

  nItems = fread(item->mem2, 1, sizeof(item->mem2), stream);
  if(nItems == 0) {
    return WORK_ERROR_NOMEM;
  }

  nItems = fread(&item->programByteCount, 1, sizeof(item->programByteCount), stream);
  if(nItems == 0) {
    return WORK_ERROR_NOBYTECOUNT;
  }

  allocateProgramSpace(item);
  nItems = fread(item->program, item->programByteCount, 1, stream);
  if(nItems < item->programByteCount) {
    return WORK_ERROR_SHORT_PROGRAM;
  }

  return WORK_ITEM;
}

// Allocate a full page (aligned to a page boundary).
//
// mmap guarantees that its result will be aligned to a page boundary, so just
// use that for simplicity.
void* allocatePage() {
  void* res = mmap(NULL, pageSize, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  // mmap doesn't return NULL on failure, because some systems can map the zero
  // page.  We don't have to worry about that on modern Linux.
  if(res == MAP_FAILED)
    return NULL;

  return res;
}

typedef void (*Program)(void);

Program mapProgram(WorkItem* item) {
  int totalBytes = item->programByteCount + sizeof(raiseTrap);
  assert(totalBytes < pageSize);

  uint8_t* program = allocatePage();
  if(!program) return NULL;

  memcpy(program, item->program, item->programByteCount);
  memcpy(program + item->programByteCount, raiseTrap, sizeof(raiseTrap));

  // Now mark the program as executable
  int res = mprotect(program,  totalBytes, PROT_READ | PROT_EXEC);
  if(!res)
    return (Program) program;

  return NULL;
}

// Apply any fixes to the context required to make it valid
void fixupContext(ucontext_t* ctx, WorkItem* item) {
  ctx->uc_mcontext = item->ctx;
  ctx->uc_stack.ss_sp = programStack;
  ctx->uc_stack.ss_flags = 0;
  ctx->uc_stack.ss_size = pageSize;
  applyMContextMask(&ctx->uc_mcontext, &item->ctxMask, item->mem1, item->mem2);
  // When we return from executing the program, we are in theory going to return
  // to restoreCtx (the main thread of execution).  That said, this link might
  // never need to be followed because we never really get to the end of that
  // context (i.e., we always throw a synchronous signal instead of returning,
  // and we jump back to restoreCtx from that signal handler).  It is set for
  // good form...
  ctx->uc_link = &restoreCtx;
}

// Take a work item, map the program (possibly with modifications), then jump to
// it.
ResponseTag processWorkItem(WorkItem* item) {
  Program p = mapProgram(item);
  if(!p) return RESPONSE_ERROR_MAPFAILED;
  ucontext_t ctx;
  fixupContext(&ctx, item);
  makecontext(&ctx, p, 0);
  swapcontext(&restoreCtx, &ctx);
  // We'll return here from the signal handler, so we can send our result back.
  // We just return a success code, and the handler code will handle writing the
  // necessary state back.
  if(signalTag == SIGNAL_SUCCESS)
    return RESPONSE_SUCCESS;
  else
    return RESPONSE_SIGNAL_ERROR;
}

// A handler for traps that captures the current CPU state into `signalCtx` and
// then tells the handler to return to `restoreCtx`.  We mark the handling as
// "successful" to mean that the test program completed without raising a
// segfault or executing an illegal instruction.
void trapHandler(int sigNum, siginfo_t* info, void* ctxp) {
  ucontext_t* ctx = ctxp;
  signalCtx = *ctx;
  *ctx = restoreCtx;
  signalTag = SIGNAL_SUCCESS;
  otherSignal = 0;

  // We don't need the signal number or detailed signal info.  We just need the
  // ucontext_t, which we can only get through this form of signal handler.
  (void)sigNum;
  (void)info;
}

// A handler for other types of signals we might catch as a result of running
// synthesized programs
void otherSigHandler(int sigNum) {
  signalTag = SIGNAL_OTHER;
  otherSignal = sigNum;
}

// Write a message indicating a read error to the channel.
//
// 1) ResponseTag (RESPONSE_READ_ERROR)
// 2) WorkTag (indicating nature of failure)
//
// Both are uint16_t
void writeReadErrorResult(FILE* stream, WorkTag wtag, const char* msg) {
  fprintf(stderr, "%s\n", msg);
  uint16_t tagBytes = RESPONSE_READ_ERROR;
  fwrite(&tagBytes, sizeof(tagBytes), 1, stream);
  tagBytes = wtag;
  fwrite(&tagBytes, sizeof(tagBytes), 1, stream);
}

// Write a response back onto the stream.  It will either be an error with
// metadata or a success with a final processor state.
void writeWorkResponse(FILE* stream, ResponseTag rtag, WorkItem* item) {
  // All responses start off with a uint16_t ResponseTag code followed by a
  // nonce
  uint16_t tagBytes = rtag;
  fwrite(&tagBytes, sizeof(tagBytes), 1, stream);
  fwrite(&item->nonce, sizeof(item->nonce), 1, stream);

  switch(rtag) {
  case RESPONSE_READ_ERROR:
    assert(0 && "Impossible, this should have been handled earlier");
  case RESPONSE_ERROR_MAPFAILED:
    break;
  case RESPONSE_SIGNAL_ERROR:
    fwrite(&otherSignal, sizeof(otherSignal), 1, stream);
    break;
  case RESPONSE_SUCCESS:
    fwrite(&signalCtx.uc_mcontext, sizeof(signalCtx.uc_mcontext), 1, stream);
    fwrite(item->mem1, sizeof(item->mem1), 1, stream);
    fwrite(item->mem2, sizeof(item->mem2), 1, stream);
    break;
  }
}

// We have to handle a few signals, so set up all of the handlers in one place
//
// We use SIGTRAP to indicate the end of the program and give us a chance to
// collect the processor state
//
// We have to also catch SIGILL (illegal instruction) in case we accidentally
// created an invalid instruction
//
// We are also on the lookout for SIGSEGV in case we accessed memory we
// shouldn't.
void setupSignalHandlers() {
  sigset_t mask;
  sigemptyset(&mask);
  struct sigaction sa;
  sa.sa_sigaction = trapHandler;
  sa.sa_mask = mask;
  sa.sa_flags = SA_SIGINFO;
  assert(!sigaction(SIGTRAP, &sa, NULL));

  sa.sa_flags = 0;
  sa.sa_handler = otherSigHandler;
  assert(!sigaction(SIGILL, &sa, NULL));
  assert(!sigaction(SIGSEGV, &sa, NULL));
}

int main(int argc, char* argv[]) {
  pageSize = sysconf(_SC_PAGESIZE);
  // This is the stack that will be used for executing the programs we get in
  // work items.  Using mmap to ensure that it is 4k aligned, though that
  // probably isn't really necessary.  The one nice thing about using mmap
  // instead of allocating the new stack on the real stack is that overflows
  // (highly unlikely) will hit an unmapped page instead of spilling onto the
  // main stack.
  programStack = allocatePage();
  assert(programStack);
  setupSignalHandlers();

  do {
    WorkItem item;
    WorkTag tag = readWorkItem(stdin, &item);
    ResponseTag rtag;
    switch(tag) {
    case WORK_DONE:
      break;
    case WORK_ERROR_NONONCE:
      writeReadErrorResult(stdout, tag, "Error while reading a work item (missing nonce)");
      break;
    case WORK_ERROR_NOCONTEXT:
      writeReadErrorResult(stdout, tag, "Error while reading a work item (missing context)");
      break;
    case WORK_ERROR_NOMEM:
      writeReadErrorResult(stdout, tag, "Error while reading a work item (missing memory block)");
      break;
    case WORK_ERROR_NOBYTECOUNT:
      writeReadErrorResult(stdout, tag, "Error while reading a work item (missing program byte count)");
      break;
    case WORK_ERROR_SHORT_PROGRAM:
      writeReadErrorResult(stdout, tag, "Error while reading a work item (missing or truncated program)");
      break;
    case WORK_ERROR_CTX_SIZE_MISMATCH:
      writeReadErrorResult(stdout, tag, "Error while reading a work item (mcontext_t size mismatch)");
      break;
    case WORK_ITEM:
      rtag = processWorkItem(&item);
      writeWorkResponse(stdout, rtag, &item);
      free(item.program);
      break;
    }
  } while(1);

  (void)argc;
  (void)argv;
  return 0;
}
