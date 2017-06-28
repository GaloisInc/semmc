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

  TODO:

  - Deal with endianness.  This is kind of problematic if we need to byte swap
    values in mcontext_t

  - Investigate alignment requirements for the stack and memory regions

  - Should we include the stack data as part of the payload?  Probably not...

 */

// The number of bytes for our hand-allocated memory regions
#define MEM_REGION_BYTES 32

typedef enum {
  WORK_ITEM = 0,
  WORK_DONE = 1,
  WORK_ERROR_NONONCE = 2,
  WORK_ERROR_NOCONTEXT = 3,
  WORK_ERROR_NOMEM = 4,
  WORK_ERROR_NOBYTECOUNT = 5,
  WORK_ERROR_SHORT_PROGRAM = 6,
  WORK_ERROR_CTX_SIZE_MISMATCH = 7
} WorkTag;

typedef enum {
  RESPONSE_SUCCESS = 0,
  RESPONSE_READ_ERROR = 1,
  RESPONSE_SIGNAL_ERROR = 2,
  // Failed to map the program into memory
  RESPONSE_ERROR_MAPFAILED = 7
} ResponseTag;

typedef enum {
  // Successful execution
  SIGNAL_SUCCESS = 0,
  // Failed with a different signal (signal number encoded separately as an int32)
  SIGNAL_OTHER = 1
} SignalTag;

// The definition of a work item; note that these must be heap allocated, as they
// will be freed
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

  // The actual bytes of the program to be executed
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
#if defined(__x86_64__)

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
      mctx->gregs[i] = (long long)mem1;
    } else if(mctxMask->gregs[i] == 2) {
      mctx->gregs[i] = (long long)mem2;
    }
  }
}


#elif defined(__arm__)

// This byte sequence encodes the `BKPT` instruction
uint8_t raiseTrap[] = {0xe1, 0x20, 0x00, 0x70};

// For any register with a value of 1 or 2, overwrite the associated slot with
// mem1 or mem2, respectively.
void applyMContextMask(mcontext_t* mctx, mcontext_t* mctxMask, uint8_t* mem1, uint8_t* mem2) {
  assert(0);
}

#endif

// Release the dynamic (program) bytes occupied by a work item
void freeWorkItem(WorkItem* item) {
  free(item->program);
  item->program = NULL;
}

/*
  Allocate space for the program in the work item based on the
  `programByteCount` field.

  If the byte count is zero, this function is a no-op.
 */
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
  7) The program to run, prefixed with a uint16_t indicating the number of bytes to read

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

typedef void (*Program)(void);

Program mapProgram(WorkItem* item) {
  int totalBytes = item->programByteCount + sizeof(raiseTrap);
  assert(totalBytes < pageSize);

  /*
    Allocate a page (we need it to be page aligned for mprotect, and mmap
    guarantees that its return is page aligned).
   */
  uint8_t* program = mmap(NULL, pageSize, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if(program == MAP_FAILED)
    return NULL;

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
  ctx->uc_link = &restoreCtx;
}

/*
  Write the results of a test back into the stream.

  This is in the same order as the input test vector; it can elide the context
  mask, since we'll know what that was already on the server side due to the
  nonce.  Likewise, we don't need to include the program.

  Note that the response needs a tag, but that is written by `writeWorkResponse`
 */
void writeTestResult(FILE* stream, WorkItem* item, ucontext_t* ctx) {
  fwrite(&item->nonce, sizeof(item->nonce), 1, stream);
  fwrite(&ctx, sizeof(ctx), 1, stream);
  fwrite(item->mem1, sizeof(item->mem1), 1, stream);
  fwrite(item->mem2, sizeof(item->mem2), 1, stream);
}

/*
  Take a work item, map the program (possibly with modifications), then jump to
  it.
 */
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

/*
  Write a message indicating a read error to the channel.

  1) ResponseTag (RESPONSE_READ_ERROR)
  2) WorkTag (indicating nature of failure)

  Both are uint16_t
 */
void writeReadErrorResult(FILE* stream, WorkTag wtag, const char* msg) {
  fprintf(stderr, "%s\n", msg);
  uint16_t tagBytes = RESPONSE_READ_ERROR;
  fwrite(&tagBytes, sizeof(tagBytes), 1, stream);
  tagBytes = wtag;
  fwrite(&tagBytes, sizeof(tagBytes), 1, stream);
}

void writeWorkResponse(FILE* stream, ResponseTag rtag, WorkItem* item) {
  // All responses start off with a uint16_t ResponseTag code
  uint16_t tagBytes = rtag;
  fwrite(&tagBytes, sizeof(tagBytes), 1, stream);

  switch(rtag) {
  case RESPONSE_READ_ERROR:
    assert(0 && "Impossible, this should have been handled earlier");
  case RESPONSE_ERROR_MAPFAILED:
    break;
  case RESPONSE_SIGNAL_ERROR:
    fwrite(&otherSignal, sizeof(otherSignal), 1, stream);
    break;
  case RESPONSE_SUCCESS:
    writeTestResult(stream, item, &signalCtx);
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
  // Set up a signal handler for SIGTRAP.  This is the magic handler that lets
  // us save contexts after execution.
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
  // work items
  programStack = mmap(NULL, pageSize, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  setupSignalHandlers();
  assert(programStack != MAP_FAILED);

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
      // Note that this doesn't free the item itself, which is stored on the
      // stack.  It only stores the heap-allocated program portion.
      freeWorkItem(&item);
      break;
    }
  } while(1);

  (void)argc;
  (void)argv;
  return 0;
}
