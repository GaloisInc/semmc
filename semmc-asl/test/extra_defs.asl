// Toplevel flags for execution status

boolean __AssertionFailure;
boolean __EndOfInstruction;
boolean __UndefinedBehavior;
boolean __UnpredictableBehavior;

array bits(32) _R[0..15];
bits(32) _PC;

PC[] = bits(32) value
    R[15] = value;
    return;

bits(32) PC[]
    return R[15];

bits(32) Rmode[integer n, bits(5) mode]
    // Check for attempted use of Monitor mode in Non-secure state.
    if !IsSecure() then assert mode != M32_Monitor;
    assert !BadMode(mode);

    if mode == M32_Monitor then
        if n == 13 then
            return SP_mon;
        elsif n == 14 then
            return LR_mon;
        elsif n == 15 then
            return _PC;
        else
            return _R[n];
    else
        idx = LookUpRIndex(n, mode);
        if idx == 15 then
            return _PC;
        else
            return _R[n];

Rmode[integer n, bits(5) mode] = bits(32) value
    assert n >= 0 && n <= 14;

    // Check for attempted use of Monitor mode in Non-secure state.
    if !IsSecure() then assert mode != M32_Monitor;
    assert !BadMode(mode);

    if mode == M32_Monitor then
        if n == 13 then
            SP_mon = value;
        elsif n == 14 then
            LR_mon = value;
        elsif n == 15 then
            _PC = value;
        else
            _R[n] = value;
    else
        idx = LookUpRIndex(n, mode);
        if idx == 15 then
            _PC = value;
        else
            _R[n] = value;
    return;

// Allow us to model the internal PC as a 32 bit value
bits(N) ThisInstrAddr()
    if N == 32 then
        return PC[];
    else
        assert FALSE;
        return bits(N) UNKNOWN;

bits(N) NextInstrAddr()
    if N == 32 then
        return (_PC + (ThisInstrLength() DIV 8))<N-1:0>;
    else
        assert FALSE;
        return bits(N) UNKNOWN;

// Allow us to model the internal PC as a 32 bit value
BranchToAddr(bits(N) target, BranchType branch_type)
    __BranchTaken = TRUE;
    Hint_Branch(branch_type);
    assert UsingAArch32();

    if N == 32 then
        PC[] = target;
        return;
    else
        assert FALSE;
        return;
    return;


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

// FIXME: This can't reasonably be simulated

integer RecipSqrtEstimate(integer a)
  assert FALSE;
  return integer UNKNOWN;

// Stubbed floating point operations to allow proper signature calculations

bits(N) FPAdd(bits(N) op1, bits(N) op2, FPCRType fpcr)
    assert FALSE;
    return bits(N) UNKNOWN;

boolean FPCompareUN(bits(N) op1, bits(N) op2, FPCRType fpcr)
    assert FALSE;
    return boolean UNKNOWN;

bits(N) FPMin(bits(N) op1, bits(N) op2, FPCRType fpcr)
    assert FALSE;
    return bits(N) UNKNOWN;

bits(N) FPProcess(bits(N) input)
    assert FALSE;
    return bits(N) UNKNOWN;

bits(N) FPTwo(bit sign)
    assert FALSE;
    return bits(N) UNKNOWN;

boolean FPCompareNE(bits(N) op1, bits(N) op2, FPCRType fpcr)
    assert FALSE;
    return boolean UNKNOWN;

bits(N) FPMinNormal(bit sign)
    assert FALSE;
    return bits(N) UNKNOWN;

boolean FPCompareGT(bits(N) op1, bits(N) op2, FPCRType fpcr)
    assert FALSE;
    return boolean UNKNOWN;

bits(N) FPSub(bits(N) op1, bits(N) op2, FPCRType fpcr)
    assert FALSE;
    return bits(N) UNKNOWN;

bits(M) FPToFixed(bits(N) op, integer fbits, boolean unsigned, FPCRType fpcr, FPRounding rounding)
    assert FALSE;
    return bits(M) UNKNOWN;

bits(N) FixedToFP(bits(M) op, integer fbits, boolean unsigned, FPCRType fpcr, FPRounding rounding)
    assert FALSE;
    return bits(N) UNKNOWN;

bits(N) FPRecpX(bits(N) op, FPCRType fpcr)
    assert FALSE;
    return bits(N) UNKNOWN;

bits(N) FPMul(bits(N) op1, bits(N) op2, FPCRType fpcr)
    assert FALSE;
    return bits(N) UNKNOWN;

bits(N) FPRecipStep(bits(N) op1, bits(N) op2)
    assert FALSE;
    return bits(N) UNKNOWN;

bits(N) FPMulAddH(bits(N) addend, bits(M) op1, bits(M) op2, FPCRType fpcr)
    assert FALSE;
    return bits(N) UNKNOWN;

bits(N) FPMinNum(bits(N) op1, bits(N) op2, FPCRType fpcr)
    assert FALSE;
    return bits(N) UNKNOWN;

bits(N) FPMax(bits(N) op1, bits(N) op2, FPCRType fpcr)
    assert FALSE;
    return bits(N) UNKNOWN;

bits(N) FPMaxNum(bits(N) op1, bits(N) op2, FPCRType fpcr)
    assert FALSE;
    return bits(N) UNKNOWN;

bits(N) FPScale(bits (N) op, integer scale, FPCRType fpcr)
    assert FALSE;
    return bits(N) UNKNOWN;

bits(N) FPRoundIntN(bits(N) op, FPCRType fpcr, FPRounding rounding, integer intsize)
    assert FALSE;
    return bits(N) UNKNOWN;

bits(4) FPCompare(bits(N) op1, bits(N) op2, boolean signal_nans, FPCRType fpcr)
    assert FALSE;
    return bits(4) UNKNOWN;

boolean FPCompareGE(bits(N) op1, bits(N) op2, FPCRType fpcr)
    assert FALSE;
    return boolean UNKNOWN;

bits(N) FPRSqrtStepFused(bits(N) op1, bits(N) op2)
    assert FALSE;
    return bits(4) UNKNOWN;

boolean FPCompareEQ(bits(N) op1, bits(N) op2, FPCRType fpcr)
    assert FALSE;
    return boolean UNKNOWN;

(boolean, bits(N)) FPProcessNaNs3(FPType type1, FPType type2, FPType type3,
                                  bits(N) op1, bits(N) op2, bits(N) op3,
                                  FPCRType fpcr)
    assert FALSE;
    return (boolean UNKNOWN, bits(N) UNKNOWN);

bits(N) FPRecipEstimate(bits(N) operand, FPCRType fpcr)
    assert FALSE;
    return bits(N) UNKNOWN;

bits(N) FPSqrt(bits(N) op, FPCRType fpcr)
    assert FALSE;
    return bits(N) UNKNOWN;

bits(M) FPConvert(bits(N) op, FPCRType fpcr, FPRounding rounding)
    assert FALSE;
    return bits(M) UNKNOWN;

bits(M) FPConvert(bits(N) op, FPCRType fpcr)
    assert FALSE;
    return bits(M) UNKNOWN;

bits(M) FPConvertSVE(bits(N) op, FPCRType fpcr, FPRounding rounding)
    assert FALSE;
    return bits(M) UNKNOWN;

bits(M) FPConvertSVE(bits(N) op, FPCRType fpcr)
    assert FALSE;
    return bits(M) UNKNOWN;

bits(N) FPDiv(bits(N) op1, bits(N) op2, FPCRType fpcr)
    assert FALSE;
    return bits(N) UNKNOWN;

bits(N) FPRSqrtEstimate(bits(N) operand, FPCRType fpcr)
    assert FALSE;
    return bits(N) UNKNOWN;

bits(N) FPTrigSMul(bits(N) op1, bits(N) op2, FPCRType fpcr)
    assert FALSE;
    return bits(N) UNKNOWN;

bits(N) FPHalvedSub(bits(N) op1, bits(N) op2, FPCRType fpcr)
    assert FALSE;
    return bits(N) UNKNOWN;

bits(N) FPRSqrtStep(bits(N) op1, bits(N) op2)
    assert FALSE;
    return bits(N) UNKNOWN;

bits(N) FPMulAdd(bits(N) addend, bits(N) op1, bits(N) op2, FPCRType fpcr)
    assert FALSE;
    return bits(N) UNKNOWN;

bits(N) FPRecipStepFused(bits(N) op1, bits(N) op2)
    assert FALSE;
    return bits(N) UNKNOWN;

bits(N) FPToFixedJS(bits(M) op, FPCRType fpcr, boolean Is64)
    assert FALSE;
    return bits(N) UNKNOWN;

bits(N) FPMulX(bits(N) op1, bits(N) op2, FPCRType fpcr)
    assert FALSE;
    return bits(N) UNKNOWN;

bits(N) FPRoundInt(bits(N) op, FPCRType fpcr, FPRounding rounding, boolean exact)
    assert FALSE;
    return bits(N) UNKNOWN;


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

