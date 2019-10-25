// Toplevel flags for execution status

boolean __AssertionFailure;
boolean __EndOfInstruction;
boolean __UndefinedBehavior;
boolean __UnpredictableBehavior;

// This flag is checked every time a function call
// might have written to it to see if we should stop
// processing the instruction early.

EndOfInstruction()
  __EndOfInstruction = TRUE;
  return;

// These are overridden in the translation to terminate
// the calling function early.

// Unlike the above flag, these are not checked on every
// function call. The resulting global state after either
// flag is tripped should be treated as undefined.

// UNDEFINED is rewritten into this
ASLSetUndefined()
  __UndefinedBehavior = TRUE;
  return;

// UNPREDICTABLE is rewritten into this
ASLSetUnpredictable()
  __UnpredictableBehavior = TRUE;
  return;

// Memory model

// Faking global reads
bits(8) read_mem_1(__RAM(52) mem, bits(52) address)
  return bits(8) UNKNOWN;

bits(16) read_mem_2(__RAM(52) mem, bits(52) address)
  return bits(16) UNKNOWN;

bits(32) read_mem_4(__RAM(52) mem, bits(52) address)
  return bits(32) UNKNOWN;

bits(64) read_mem_8(__RAM(52) mem, bits(52) address)
  return bits(64) UNKNOWN;

bits(128) read_mem_16(__RAM(52) mem, bits(52) address)
  return bits(128) UNKNOWN;

// Faking global writes
write_mem_1(__RAM(52) mem, bits(52) address, bits(8) value)
  return;

write_mem_2(__RAM(52) mem, bits(52) address, bits(16) value)
  return;

write_mem_4(__RAM(52) mem, bits(52) address, bits(32) value)
  return;

write_mem_8(__RAM(52) mem, bits(52) address, bits(64) value)
  return;

write_mem_16(__RAM(52) mem, bits(52) address, bits(128) value)
  return;


// Translator overrides dispatch to the correctly-named variant based on the concrete
// value of the given size
bits(8*size) _Mem[AddressDescriptor desc, integer size, AccessDescriptor accdesc]
    assert size IN {1, 2, 4, 8, 16};
    bits(52) address = desc.paddress.address;
    assert address == Align(address, size);
    return read_mem(__Memory, address, size);

_Mem[AddressDescriptor desc, integer size, AccessDescriptor accdesc] = bits(8*size) value
    assert size IN {1, 2, 4, 8, 16};
    bits(52) address = desc.paddress.address;
    assert address == Align(address, size);
    write_mem(__Memory, address, size, value);
    return;

// Constant lifted from mra_tools

constant integer LOG2_TAG_GRANULE=4;
constant integer TAG_GRANULE=2 ^ LOG2_TAG_GRANULE;

// Bitvector primitives

bits(N*M) Replicate(bits(N) bv)
    return Replicate(bv, M);

integer sizeOf(bits(N) bv)
    return N;


bits(width) BigEndianReverse (bits(width) value)
    assert width IN {8, 16, 32, 64, 128};
    integer half = width DIV 2;
    StaticBind(half, width DIV 2); // hint to resolve dependent type
    if width == 8 then return value;
    return BigEndianReverse(value<half-1:0>) : BigEndianReverse(value<width-1:half>);

// Shifting Overrides

// These should be semantically equivalent to the functions in
// the standard ASL spec, but have been rewritten to not require
// intermediate arbitrarily-sized bitvectors.

(bits(N), bit) LSL_C(bits(N) x, integer shift)
    assert shift > 0;
    shift = if shift > N then N else shift;
    carry_out = x<N - shift>;
    result = LSL(x, shift);
    return (result, carry_out);

bits(N) LSL(bits(N) x, integer shift)
    assert shift >= 0;
    shift = if shift > N then N else shift;
    if shift == 0 then
        result = x;
    else
        result = x << shift;
    return result;

(bits(N), bit) LSR_C(bits(N) x, integer shift)
    assert shift > 0;
    shift = if shift > N then N else shift;
    carry_out = x<shift-1>;
    result = LSR(x, shift);
    return (result, carry_out);

bits(N) LSR(bits(N) x, integer shift)
    assert shift >= 0;
    if shift == 0 then
        result = x;
    else
        result = x >> shift;
    return result;

(bits(N), bit) ASR_C(bits(N) x, integer shift)
    assert shift > 0;
    shift = if shift > N then N else shift;
    carry_out = x<shift-1>;
    result = ASR(x, shift);
    return (result, carry_out);

bits(N) ASR(bits(N) x, integer shift)
    assert shift >= 0;
    shift = if shift > N then N else shift;
    if shift == 0 then
        result = x;
    else
        result = primitive_ASR(x, shift);
    return result;


// We assume that the MMU is disabled and that general address translation
// is not going to occur. These functions appear to be too complex to translate.

TLBRecord AArch64.TranslationTableWalk(bits(52) ipaddress, bit s1_nonsecure, bits(64) vaddress,
                                       AccType acctype, boolean iswrite, boolean secondstage,
                                       boolean s2fs1walk, integer size)
  assert FALSE;
  TLBRecord result;
  return result;

TLBRecord AArch32.TranslationTableWalkLD(bits(40) ipaddress, bits(32) vaddress,
                                         AccType acctype, boolean iswrite, boolean secondstage,
                                         boolean s2fs1walk, integer size)
  assert FALSE;
  TLBRecord result;
  return result;

TLBRecord AArch32.TranslationTableWalkSD(bits(32) vaddress, AccType acctype, boolean iswrite,
                                         integer size)
  assert FALSE;
  TLBRecord result;
  return result;

// Misc stubs for system and debug functions

ConsumptionOfSpeculativeDataBarrier()
    return;

SpeculativeStoreBypassBarrierToPA()
    return;

SpeculationBarrier()
    return;

SpeculativeStoreBypassBarrierToVA()
    return;

SynchronizeErrors()
    return;

bits(64) AArch64.SysRegRead(integer op0, integer op1, integer crn, integer crm, integer op2)
    assert FALSE;
    reg = bits(64) UNKNOWN;
    return reg;

ReservedEncoding()
    return;

boolean IsPhysicalSErrorPending()
    ret = boolean UNKNOWN;
    return ret;

(boolean,boolean) AArch32.BreakpointValueMatch(integer n, bits(32) vaddress, boolean linked_to)
  return (FALSE, FALSE);

boolean AArch64.BreakpointValueMatch(integer n, bits(64) vaddress, boolean linked_to)
  return FALSE;

boolean IsBlockDescriptorNTBitValid()
  return FALSE;

bits(11) LSInstructionSyndrome()
  assert FALSE;
  ret = bits(11) UNKNOWN;
  return ret;

TraceSynchronizationBarrier()
  assert FALSE;
  return;

__abort()
  assert FALSE;
  return;

boolean AArch32.WatchpointMatch(integer n, bits(32) vaddress, integer size, boolean ispriv,
                                 boolean iswrite)
  return FALSE;

boolean AArch64.WatchpointMatch(integer n, bits(64) vaddress, integer size, boolean ispriv,
                                AccType acctype, boolean iswrite)
  return FALSE;

