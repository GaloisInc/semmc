#include <assert.h>
#include <errno.h>
#include <execinfo.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>

#include <arpa/inet.h>
#include <elf.h>
#include <signal.h>
#include <sys/mman.h>
#include <sys/procfs.h>
#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/user.h>
#include <sys/wait.h>

/*
  This program is designed to be a remote runner of programs with test vectors.
  It is based on ptrace and is two processes: a test orchestrator and a test
  runner.  The test runner is trivial (see runTests).  The test orchestrator
  uses ptrace to set up test states, allow the runner to run them, and then
  inspect the state after the test.  The test orchestrator (see traceChild):

  1) reads a test vector and test program off of stdin,
  2) sets up the initial test state (program + registers + memory), splicing in code at the end to raise a SIGTRAP,
  3) catches the SIGTRAP and takes a snapshot of the register and memory state, and
  4) sends the result out over stdout.


  Notes:

  * We don't deal with endianness in the register states.  It is the responsibility
    of the test generator to send data in the correct endianness.  Lengths that
    are inspected are expected to come in network byte order and are corrected
    to host network order as appropriate.

  * The state of the program stack isn't tracked for test programs (though the stack pointer is)

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

#define LOG_FILE "/tmp/remote-runner.log"
#if defined(LOGGING)

FILE* logStream = NULL;
#define LOG(...) fprintf(stderr, __VA_ARGS__)
void initializeLogger() {
  /* logStream = fopen(LOG_FILE, "w"); */
  /* assert(logStream); */
  /* setlinebuf(logStream); */
  LOG("Log initialized\n");
}

#else

#define LOG(...)
void initializeLogger() {
}

#endif


void checkedPtrace(enum __ptrace_request rq, pid_t pid, void* addr, void* data) {
  long res = ptrace(rq, pid, addr, data);
  if(!res)
    return;

  LOG("ptrace call (rq=%d) failed: %s\n", rq, strerror(errno));
  void* bt[128];
  int nframes = backtrace(bt, 128);
  char** symbols = backtrace_symbols(bt, nframes);
  for(int i = 0; i < nframes; ++i) {
    LOG("%s\n", symbols[i]);
  }
  free(symbols);

  exit(-1);
}

/*
  Architecture configuration

  Five things are required:

  1) A `RegisterState` struct type that includes memory regions

  2) `raiseTrap`, an array of bytes encoding a trap instruction that will cause
     the processor to raise a SIGTRAP when executed

  3) `RAISE_TRAP`, a macro that expands to inline assembly raising a SIGTRAP

  4) `setupRegisterState`, which takes a `RegisterState` and uses ptrace to set
     up a test vector on the target process (tracee)

  5) `snapshotRegisterState`, which uses ptrace to read out a `RegisterState`

  The last two are non-trivial because it requires multiple ptrace calls to
  assemble the required information.  That information varies greatly by
  platform.
 */
#if defined(__x86_64__) || defined(__i386__)

#if defined(__x86_64__)

#include <sys/reg.h>

// The standard general purpose integer registers
#define SEM_NGPRS 16
// The MM registers (for MMX), which overlay the x87 floating point stack
#define SEM_NFPRS 8
// The 16 YMM registers (the lower halves of which are the XMM registers)
//
// Note, this doesn't support the ZMM registers from AVX2 yet
#define SEM_NVECREGS 16

uint8_t raiseTrap[] = {0x7f, 0xe0, 0x00, 0x08};
#define RAISE_TRAP asm("trap")

typedef struct {
  uint64_t chunks[4];
} YMM;

typedef struct {
  uint64_t gprs[SEM_NGPRS];
  uint64_t gprs_mask[SEM_NGPRS];
  uint64_t eflags;
  uint64_t fprs[SEM_NFPRS];
  YMM vrs[SEM_NVECREGS];
  uint8_t mem1[MEM_REGION_BYTES];
  uint8_t mem2[MEM_REGION_BYTES];
} RegisterState;

#else

// The standard general purpose integer registers
#define SEM_NGPRS 8
// The MM registers (for MMX), which overlay the x87 floating point stack
#define SEM_NFPRS 8
// The 16 YMM registers (the lower halves of which are the XMM registers)
#define SEM_NVECREGS 8

typedef struct {
  uint32_t chunks[4];
} XMM;

typedef struct {
  uint32_t gprs[SEM_NGPRS];
  uint32_t gprs_mask[SEM_NGPRS];
  uint32_t eflags;
  uint64_t fprs[SEM_NFPRS];
  XMM vrs[SEM_NVECREGS];
} RegisterState;


#endif

// Raise a trap with INT 3
uint8_t raiseTrap[] = {0xcc};

#define RAISE_TRAP asm("int3")

#if defined(__x86_64__)
#define CAST_PTR(x) ((unsigned long long)(x))

/*
  Do we want to change WorkItem to have fixed-size register buffers?  It would
  simplify some things.  We would just have to float the arch-specific defs up
  before WorkItem.
 */

// Install the expected register and memory state from the work item.
//
// This requires that the tracee must be stopped.
void setupRegisterState(pid_t childPid, uint8_t* programSpace, uint8_t* memSpace, RegisterState* rs) {
  struct user_regs_struct regs;
  struct iovec iov;
  iov.iov_base = &regs;
  iov.iov_len = sizeof(regs);
  checkedPtrace(PTRACE_GETREGSET, childPid, (void*)NT_PRSTATUS, &iov);

  // Set up the provided memory state
  //
  // We have to copy the memory from the input buffers into memSpace because the
  // input buffers aren't visible in the address space of the tracee - only the
  // memSpace is shared.
  uint8_t* mem1Addr = memSpace;
  uint8_t* mem2Addr = memSpace + sizeof(rs->mem1);
  memcpy(mem1Addr, rs->mem1, sizeof(rs->mem1));
  memcpy(mem2Addr, rs->mem2, sizeof(rs->mem2));

  // Apply the reg mask; this modifies the test vector, but that is fine.  We
  // won't need the original values ever again.
  for(int i = 0; i < SEM_NGPRS; ++i) {
    if(rs->gprs_mask[i] == 1)
      rs->gprs[i] = CAST_PTR(mem1Addr);
    else if(rs->gprs_mask[i] == 2)
      rs->gprs[i] = CAST_PTR(mem2Addr);
  }

  regs.eflags =   rs->eflags;
  regs.rax =   rs->gprs[0];
  regs.rbx =   rs->gprs[1];
  regs.rcx =   rs->gprs[2];
  regs.rdx =   rs->gprs[3];
  regs.rdi =   rs->gprs[4];
  regs.rsi =   rs->gprs[5];
  regs.rbp =   rs->gprs[6];
  regs.rsp =   rs->gprs[7];
  regs.r8 =   rs->gprs[8];
  regs.r9 =   rs->gprs[9];
  regs.r10 =   rs->gprs[10];
  regs.r11 =   rs->gprs[11];
  regs.r12 =   rs->gprs[12];
  regs.r13 =   rs->gprs[13];
  regs.r14 =   rs->gprs[14];
  regs.r15 =   rs->gprs[15];

  // Finally, set the IP to be at the start of our test program
  regs.rip = CAST_PTR(programSpace);
  iov.iov_len = sizeof(regs);
  LOG("PTRACE_SETREGSET: setting RIP to %llx\n", regs.rip);
  checkedPtrace(PTRACE_SETREGSET, childPid, (void*)NT_PRSTATUS, &iov);
}
// Extract the register state via ptrace and copy the memory values.
//
// This requires that the tracee be stopped
void snapshotRegisterState(pid_t childPid, uint8_t* memSpace, RegisterState* rs) {
  struct user_regs_struct regs;
  struct iovec iov;
  iov.iov_base = &regs;
  iov.iov_len = sizeof(regs);
  checkedPtrace(PTRACE_GETREGSET, childPid, (void*)NT_PRSTATUS, &iov);
  rs->eflags = regs.eflags;
  rs->gprs[0] = regs.rax;
  rs->gprs[1] = regs.rbx;
  rs->gprs[2] = regs.rcx;
  rs->gprs[3] = regs.rdx;
  rs->gprs[4] = regs.rdi;
  rs->gprs[5] = regs.rsi;
  rs->gprs[6] = regs.rbp;
  rs->gprs[7] = regs.rsp;
  rs->gprs[8] = regs.r8;
  rs->gprs[9] = regs.r9;
  rs->gprs[10] = regs.r10;
  rs->gprs[11] = regs.r11;
  rs->gprs[12] = regs.r12;
  rs->gprs[13] = regs.r13;
  rs->gprs[14] = regs.r14;
  rs->gprs[15] = regs.r15;

  // Also save the memory state back into rs
  memcpy(&rs->mem1, memSpace, sizeof(rs->mem1));
  memcpy(&rs->mem2, memSpace + sizeof(rs->mem1), sizeof(rs->mem2));
}

#undef CAST_PTR
#elif defined(__i386__)

#define CAST_PTR(x) ((unsigned long)(x))

#endif
#elif defined(__arm__)

#elif defined(__aarch64__)

#elif defined(__powerpc__)

#define CAST_PTR(x) ((uintptr_t) (x))

#if defined(__powerpc64__)
#error "PPC64 not supported yet"
#else

// The standard general purpose integer registers
#define SEM_NGPRS 32
#define SEM_NVRS 32
#define SEM_NFPRS 32

typedef struct {
  uint64_t chunks[2];
} VR;

typedef struct {
  uint32_t gprs[SEM_NGPRS];
  uint32_t gprs_mask[SEM_NGPRS];
  uint32_t msr;
  uint32_t ctr;
  uint32_t link;
  uint32_t xer;
  uint32_t cr;
  uint64_t fprs[SEM_NFPRS];
  uint64_t fpscr;
  VR vrs[SEM_NVSRS];
  uint8_t mem1[MEM_REGION_BYTES];
  uint8_t mem2[MEM_REGION_BYTES];
} RegisterState;

#define VRREGS_SIZE (33 * sizeof(VR) + sizeof(uint32_t))

void setupRegisterState(pid_t childPid, uint8_t *programSpace, uint8_t *memSpace, RegisterState *rs) {
  struct pt_regs regs;

  checkedPtrace(PTRACE_GETREGS, childPid, 0, (void *) &regs);

  // Set up the provided memory state
  //
  // We have to copy the memory from the input buffers into memSpace because the
  // input buffers aren't visible in the address space of the tracee - only the
  // memSpace is shared.
  uint8_t* mem1Addr = memSpace;
  uint8_t* mem2Addr = memSpace + sizeof(rs->mem1);
  memcpy(mem1Addr, rs->mem1, sizeof(rs->mem1));
  memcpy(mem2Addr, rs->mem2, sizeof(rs->mem2));

  // Apply the reg mask; this modifies the test vector, but that is fine.  We
  // won't need the original values ever again.
  for(int i = 0; i < SEM_NGPRS; ++i) {
    if(rs->gprs_mask[i] == 1)
      rs->gprs[i] = CAST_PTR(mem1Addr);
    else if(rs->gprs_mask[i] == 2)
      rs->gprs[i] = CAST_PTR(mem2Addr);
  }

  for (int i = 0; i < SEM_NGPRS; i++) {
    regs.gprs[i] = rs->gprs[i];
  }
  regs.msr  = rs->msr;
  regs.ctr  = rs->ctr;
  regs.link = rs->link;
  regs.xer  = rs->xer;
  regs.ccr  = rs->cr;

  // Set the IP to be at the start of our test program
  regs.nip = rs->ip;
  LOG("PTRACE_SETREGS: setting IP to %" PRIxPTR "\n", regs.nip);

  checkedPtrace(PTRACE_SETREGS, childPid, 0, &regs);

  elf_fpregset_t fpregs;

  checkedPtrace(PTRACE_GETFPREGS, childPid, 0, (void *) &fpregs);

  // Copy in the FP regs
  for (int i = 0; i < SEM_NFPRS; i++) {
    fpregs[i] = *((const double *) &rs->fprs[i]);
  }
  fpregs[SEM_NFPRS] = *((const double *) &rs->fpscr);

  checkedPtrace(PTRACE_SETFPREGS, childPid, 0, (void *) &fpregs);

  // Anonymous struct to ensure proper alignment
  struct {
    VR vrregs[SEM_NVRS];
    VR vrstatus;
    uint32_t vrweird;
  } vrbuf;

  checkedPtrace(PTRACE_GETVRREGS, childPid, 0, (void *) &vrbuf);

  // Copy in the VR regs
  for (int i = 0; i < SEM_NVRS; i++) {
    vrbuf.vrregs[i] = vs->vrs[i];
  }

  checkedPtrace(PTRACE_SETVRREGS, childPid, 0, (void *) &vrBuf);
}

void snapshotRegisterState(pid_t childPid, uint8_t* memSpace, RegisterState* rs) {
  struct pt_regs regs;

  checkedPtrace(PTRACE_GETREGS, childPid, 0, (void *) &regs);

  for (int i = 0; i < SEM_NGPRS; i++) {
    rs->gprs[i] = regs.gprs[i];
  }
  rs->msr = regs.msr;
  rs->ctr = regs.ctr;
  rs->link = regs.link;
  rs->xer = regs.xer;
  rs->cr = regs.ccr;

  elf_fpregset_t fpregs;

  checkedPtrace(PTRACE_GETFPREGS, childPid, 0, (void *) &fpregs);

  // Copy in the FP regs
  for (int i = 0; i < SEM_NFPRS; i++) {
    rs->fprs[i] = *((const uint64_t *) &fpregs[i]);
  }
  rs->fpscr = *((const uint64_t *) &fpregs[SEM_NFPRS]);

  // Anonymous struct to ensure proper alignment
  struct {
    VR vrregs[SEM_NVRS];
    VR vrstatus;
    uint32_t vrweird;
  } vrbuf;

  checkedPtrace(PTRACE_GETVRREGS, childPid, 0, (void *) &vrbuf);

  // Copy in the VR regs
  for (int i = 0; i < SEM_NVRS; i++) {
    vs->vrs[i] = vrbuf.vrregs[i];
  }
}

#endif

#endif

typedef enum {
  WORK_ITEM = 0,
  WORK_DONE = 1,
  // Possible parse errors
  WORK_ERROR_NONONCE = 2,
  WORK_ERROR_NOCONTEXT = 3,
  WORK_ERROR_NOMEM = 4,
  WORK_ERROR_NOBYTECOUNT = 5,
  WORK_ERROR_SHORT_PROGRAM = 6,
  WORK_ERROR_CTX_SIZE_MISMATCH = 7,
  WORK_ERROR_FORK_FAILED = 8,
  WORK_ERROR_REGSTATE_SIZE_ERROR = 9
} WorkTag;

typedef enum {
  RESPONSE_SUCCESS = 0,
  // Error reading a WorkItem (followed by a WorkTag as a uint16_t)
  RESPONSE_READ_ERROR = 1,
  // Received a SIGILL or SIGSEGV (followed by an int32_t with the signal number)
  RESPONSE_SIGNAL_ERROR = 2
} ResponseTag;

// The definition of a work item
typedef struct {
  // The unique identifier of this work item.  It only needs to be unique in this process.
  uint64_t nonce;

  // The register (and memory) state for the test vector
  RegisterState regs;

  // The number of bytes occupied by the test program
  uint16_t programByteCount;

  // The actual bytes of the program to be executed; note that this is
  // dynamically allocated, and must be freed with `freeWorkItem`.
  uint8_t* program;
} WorkItem;


// The signal number received in the handler (probably either segv or sigill)
int32_t otherSignal;
// The size of a page in the system.  We store this as a global so we only need
// to compute it once.
int pageSize = 0;


// Allocate space for the program in the work item based on the
// `programByteCount` field.
//
// If the byte count is zero, this function is a no-op.
void allocateProgramSpace(WorkItem* item) {
  if(item->programByteCount == 0)
    return;

  item->program = calloc(item->programByteCount, 1);
}

// Wait for a signal and return the signal number
//
// Returns -1 on error (if wait failed or if the process was terminated instead
// of stopped).
int waitForSignal(pid_t childPid) {
  int wstatus;
  int res = waitpid(childPid, &wstatus, 0);
  if(res == -1)
    return -1;

  if(WIFSTOPPED(wstatus)) {
    return WSTOPSIG(wstatus);
  }

  return -1;
}

/*
  Read a work item off of the stream (or report an error/end of work).

  The first thing we do is check the type of work coming at us (either a marker
  indicating that we are done, or any other value, which indicates that we are
  expecting a work item).

  The work format will be:

  1) The work item nonce (uint64_t)
  2) The size of the register state as a uint16_t (verified against the size expected in this process)
  3) A register context (platform specific)
  4) A uint16_t denoting the number of bytes in the test program
  5) The program to run
 */
WorkTag readWorkItem(FILE* stream, WorkItem* item) {
  LOG("readWorkItem\n");
  uint8_t tagByte;
  size_t nItems = fread(&tagByte, 1, sizeof(tagByte), stream);
  LOG("  tagByte = %d\n", tagByte);
  if(nItems == 0 || tagByte == WORK_DONE)
    return WORK_DONE;


  // Note that we don't byte swap the nonce since it doesn't really matter on
  // this end
  nItems = fread(&item->nonce, 1, sizeof(item->nonce), stream);
  if(nItems == 0) {
    return WORK_ERROR_NONONCE;
  }

  LOG("  nonce = %lu\n", item->nonce);

  uint16_t regStateBytes;
  nItems = fread(&regStateBytes, 1, sizeof(regStateBytes), stream);
  regStateBytes = ntohs(regStateBytes);
  if(nItems == 0) {
    if(regStateBytes != sizeof(item->regs)) {
      fprintf(stderr, "Register state size mismatch (expected %lu but got %d)\n", sizeof(item->regs),  regStateBytes);
      return WORK_ERROR_REGSTATE_SIZE_ERROR;
    }

    return WORK_ERROR_NOCONTEXT;
  }

  LOG("  expecting %hu bytes of context\n", regStateBytes);

  nItems = fread(&item->regs, 1, sizeof(item->regs), stream);
  if(nItems == 0)
    return WORK_ERROR_NOCONTEXT;

  nItems = fread(&item->programByteCount, 1, sizeof(item->programByteCount), stream);
  if(nItems == 0) {
    return WORK_ERROR_NOBYTECOUNT;
  }
  item->programByteCount = ntohs(item->programByteCount);

  LOG(" expecting %hu bytes of program\n", item->programByteCount);

  allocateProgramSpace(item);
  nItems = fread(item->program, item->programByteCount, 1, stream);
  if(nItems != 1) {
    return WORK_ERROR_SHORT_PROGRAM;
  }

  return WORK_ITEM;
}

// Allocate a full page (aligned to a page boundary).
//
// mmap guarantees that its result will be aligned to a page boundary, so just
// use that for simplicity.
void* allocatePage(int sharedOrPrivate) {
  void* res = mmap(NULL, pageSize, PROT_READ | PROT_WRITE, sharedOrPrivate | MAP_ANONYMOUS, -1, 0);
  // mmap doesn't return NULL on failure, because some systems can map the zero
  // page.  We don't have to worry about that on modern Linux.
  if(res == MAP_FAILED)
    return NULL;

  return res;
}

// Copy the program into our memory region (followed by a trap).
//
// We don't change any permissions because the tracee already has it mapped as
// executable.
void mapProgram(uint8_t* programSpace, WorkItem* item) {
  // Zero out the page before we continue.
  memset(programSpace, 0, pageSize);
  int totalBytes = item->programByteCount + sizeof(raiseTrap);
  assert(totalBytes < pageSize);

  memcpy(programSpace, item->program, item->programByteCount);
  memcpy(programSpace + item->programByteCount, raiseTrap, sizeof(raiseTrap));
}

// Take a work item, map the program (possibly with modifications), then jump to
// it.
ResponseTag processWorkItem(pid_t childPid, uint8_t* programSpace, uint8_t* memSpace, WorkItem* item, RegisterState* postState) {
  int sig;

  mapProgram(programSpace, item);
  setupRegisterState(childPid, programSpace, memSpace, &item->regs);
  LOG("Sending SIGCONT\n");
  checkedPtrace(PTRACE_CONT, childPid, NULL, NULL);
  sig = waitForSignal(childPid);
  LOG("Received signal %d\n", sig);
  // FIXME: This is all pretty unsatisfying.  Eliminate the global and be more
  // specific in the error condition if we get a different return value from
  // waitForSignal.
  if(sig == SIGTRAP) {
    LOG("Taking a state snapshot\n");
    snapshotRegisterState(childPid, memSpace, postState);
    return RESPONSE_SUCCESS;
  }
  else if(sig == SIGSEGV || sig == SIGILL) {
    otherSignal = sig;
    return RESPONSE_SIGNAL_ERROR;
  }
  else
    return RESPONSE_SIGNAL_ERROR;
}

// Write a message indicating a read error to the channel.
//
// 1) ResponseTag (RESPONSE_READ_ERROR)
// 2) WorkTag (indicating nature of failure)
//
// Both are uint16_t
void writeReadErrorResult(FILE* stream, WorkTag wtag, const char* msg) {
  LOG("%s\n", msg);
  fprintf(stderr, "%s\n", msg);
  uint8_t tagByte = RESPONSE_READ_ERROR;
  fwrite(&tagByte, sizeof(tagByte), 1, stream);
  tagByte = wtag;
  fwrite(&tagByte, sizeof(tagByte), 1, stream);

  fflush(stream);
}

// Write a response back onto the stream.  It will either be an error with
// metadata or a success with a final processor state.
void writeWorkResponse(FILE* stream, ResponseTag rtag, WorkItem* item, RegisterState* postState) {
  LOG("Writing a work response with tag %d\n", rtag);
  // All responses start off with a uint16_t ResponseTag code followed by a
  // nonce
  uint8_t tagByte = rtag;
  fwrite(&tagByte, sizeof(tagByte), 1, stream);
  fwrite(&item->nonce, sizeof(item->nonce), 1, stream);

  switch(rtag) {
  case RESPONSE_READ_ERROR:
    assert(0 && "Impossible, this should have been handled earlier");
  case RESPONSE_SIGNAL_ERROR: {
    int32_t sigBytes = htonl(otherSignal);
    fwrite(&sigBytes, sizeof(sigBytes), 1, stream);
    break;
  }
  case RESPONSE_SUCCESS: {
    LOG("RESPONSE_SUCCESS (context size = %lu)\n", sizeof(*postState));
    uint16_t szBytes = htons(sizeof(*postState));
    fwrite(&szBytes, sizeof(szBytes), 1, stream);
    fwrite(postState, sizeof(*postState), 1, stream);
    break;
  }
  }

  fflush(stream);
}

// Free all the dynamically allocated parts of a WorkItem
void freeWorkItem(WorkItem* item) {
  free(item->program);
}

/*
  Wait for the first trap, then enter the work loop to receive work items and
  then execute the test vectors.

  After that, accept program states over a pipe from the tracee (which will then
  stop with a trap).  Set up the requested states and then alter the IP of the
  tracee to the address of the function (which is also passed over the pipe).
  The tracer never needs to communicate back to the tracee, so the one-way pipe
  is fine.
 */
int traceChild(pid_t childPid, uint8_t* programSpace, uint8_t* memSpace) {
  LOG("Tracing child process %d\n", childPid);
  int sig = waitForSignal(childPid);
  if(sig != SIGTRAP)
    return -1;
  LOG("Child process stopped with SIGTRAP\n");

  do {
    // The invariant in this loop is that the tracee is stopped at the loop entry.
    //
    // We enforce this manually for the first iteration (since the tracee is set
    // up to trap immediately, we can start off by waiting on it).
    //
    // For all other loop iterations, the tracee is stopped because the test
    // program traps when it is done (or halts due to another signal) so that we
    // can capture its state.  We don't resume it until we have a work item (the
    // WORK_ITEM case below).
    WorkItem item;
    memset(&item, 0, sizeof(item));
    WorkTag tag = readWorkItem(stdin, &item);
    ResponseTag rtag;
    switch(tag) {
    case WORK_DONE:
      LOG("Work done\n");
      return 0;
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
    case WORK_ERROR_REGSTATE_SIZE_ERROR:
      writeReadErrorResult(stdout, tag, "Invalid RegisterState size");
      break;
    case WORK_ERROR_FORK_FAILED:
      writeReadErrorResult(stdout, tag, "Fork failed");
      break;
    case WORK_ITEM: {
      LOG("Got a work item with nonce %lu\n", item.nonce);
      RegisterState postState;
      memset(&postState, 0, sizeof(postState));
      rtag = processWorkItem(childPid, programSpace, memSpace, &item, &postState);
      writeWorkResponse(stdout, rtag, &item, &postState);
      break;
    }
    }
    freeWorkItem(&item);
  } while(1);
}

/*
  Do whatever initial setup is required and then pause so that the tracer can
  capture the necessary state to be able to reset to the start of the work loop.

  The loop is implicit: it runs from the explicit trap here until the trap at
  the end of the work item (stored in programSpace).  The tracer manages the
  loop by dropping us back at the beginning of programSpace for each new test.
 */
void runTests(uint8_t* programSpace) {
  int mpres = mprotect(programSpace, pageSize, PROT_EXEC | PROT_READ);
  assert(!mpres);
  // We always want to return after this point once we receive a trap from the
  // test program.  This should just work out, as the IP will be incremented
  // before the trap is run, so we can just capture the entire state here.
  //
  // Note that we don't really need any code here: we'll always be stopping and
  // restarting at the beginning of the shared code page, which isn't really
  // visible here.
  RAISE_TRAP;
}

int main(int argc, char* argv[]) {
  (void)argc;
  (void)argv;
  pid_t childPid;
  uint8_t* memSpace = NULL;
  uint8_t* programSpace = NULL;

  pageSize = sysconf(_SC_PAGESIZE);

  // Allocate space for our test vectors in shared memory (before we fork) so
  // both the tracer and tracee can read them (and at the same address).
  memSpace = allocatePage(MAP_SHARED);
  programSpace = allocatePage(MAP_SHARED);

  assert(memSpace);
  assert(programSpace);

  childPid = fork();
  if(childPid == -1) {
    writeReadErrorResult(stdout, WORK_ERROR_FORK_FAILED, "Failed to fork runner");
    return -1;
  } else if(childPid == 0) {
    // This is the child process that will map programs and generate SIGTRAPs
    ptrace(PTRACE_TRACEME, 0, NULL, NULL);
    runTests(programSpace);
    return 0;
  } else {
    initializeLogger();
    // This is the parent process that will handle SIGTRAPs (and other signals)
    // to capture state.
    return traceChild(childPid, programSpace, memSpace);
  }
}
