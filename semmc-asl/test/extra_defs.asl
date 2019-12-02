// Shadowed arithmetic operations

bits(N) BVMul(bits(N) bv1, bits(N) bv2)
    return primitive(bv1 * bv2);

integer IntMul(integer i1, integer i2)
    return primitive(i1 * i2);

integer IntMod(integer i1, integer i2)
    return primitive(i1 MOD i2);

integer IntDiv(integer i1, integer i2)
    return primitive(i1 / i2);

// Undefined values. UNKNOWN actually loops back to just calling these, so
// we need to tie this off somewhere.

bits(N) UNDEFINED_bitvector()
    return bits(N) UNKNOWN;

integer UNDEFINED_integer()
    return integer UNKNOWN;

boolean UNDEFINED_boolean()
    return boolean UNKNOWN;


// Unclear how to actually determine the register index
(bits(32), bits(4)) unpackRegister(bits(32) register)
    bits(32) realRegister = register;
    bits(4) registerIdx = bits(4) UNKNOWN;
    return (realRegister, registerIdx);

// Throw away the register index currently
bits(32) packRegister(bits(32) realRegister, bits(4) registerIdx)
    return realRegister;

// Defining slicing with primitive bitvector operations

bits(M) truncate(bits(N) bv, integer M);

// The target length may be larger than hi - lo, with
// the expectation that the resulting bitvector must be
// either zero or sign-extended (according to the signed flag) to the
// target length.

bits(length) getSlice(bits(N) inbv, boolean signed, integer lo, integer hi)
    assert length <= N;
    assert length >= 1;
    assert hi >= lo;
    assert hi <= length;
    assert lo >= 0;
    assert (hi - lo) <= length;

    bits(N) bv = inbv;
    // bv = [ bv_(N-1) .. bv_hi(hi) .. bv_lo(lo) .. bv_0](N)
    if signed then
        bv = primitive_ASR(bv, lo);
        // bv = [ 1 1 .. bv_hi(hi-lo) .. bv_lo(0) ] (N)
    else
        bv = bv >> lo;
        // bv = [ 0 0 .. bv_hi(hi-lo) .. bv_lo(0) ] (N)

    // where S == 1 iff signed
    // bv = [ S S .. bv_hi(hi-lo) .. bv_lo(lo) ] (N)
    bits(N) mask = Ones(N);
    // mask = [ 1 1 .. 1 1 ] (N)
    mask = NOT (mask << length);
    // mask = [ 0 0 .. 1(length) .. 1(0) ] (N)
    bv = bv AND mask;
    // bv = [ 0 0 .. S(length) S .. bv_hi(hi - lo) .. bv_lo(0) ] (N)
    return truncate(bv, length);
    // [ S(length) S .. bv_hi(hi - lo) .. bv_lo(0) ] (length)

// The length of the input bitvector may be larger than
// the range we are setting, in which case we simply drop
// any bits above hi - lo.

bits(N) setSlice(bits(N) basebv, integer lo, integer hi, bits(length) asnbv)
    assert length <= N;
    assert length >= 1;
    assert hi >= lo;
    assert hi <= length;
    assert lo >= 0;
    assert (hi - lo) <= length;

    bits(length) bv = asnbv;
    // bv = [bv(length) .. bv_hi(hi) .. bv_0(0)](length)

    bv = bv << (length - hi);
    // bv = [ bv_hi (length) .. bv_0(length - hi) .. 0 0 ](length)

    bv = bv >> (length - hi);
    // bv = [ 0 0 .. bv_hi(hi) .. bv_0(0)](length)

    ebv = ZeroExtend(NOT(bv), N);
    // ebv = [0 0 0 .. -bv_hi(hi) .. -bv_0(0)](N)

    ebv = NOT(ebv << lo);
    // ebv = [1 1 .. bv_hi(hi + lo) .. bv_0(lo) .. 1](N)

    result = basebv AND ebv;
    // [basebv_(N-1) .. bv_hi(hi + lo) .. bv_0(lo) .. basebv_0](N)
    return result;


// Toplevel flags for execution status

boolean __AssertionFailure;
boolean __EndOfInstruction;
boolean __UndefinedBehavior;
boolean __UnpredictableBehavior;

array bits(32) _R[0..15];
bits(32) _PC;

PC[] = bits(32) value
    _PC = value;
    return;

bits(32) PC[]
    return _PC;

//Consistent treatment for GPRs and PC
bits(32) RGen[integer n]
    if n == 15 then
        return _PC;
    else
        return R[n];

RGen[integer n] = bits(32) value
    if n == 15 then
        _PC = value;
    else
        R[n] = value;

bits(32) Rmode[integer n, bits(5) mode]
    assert n >= 0 && n <= 14;
    // Check for attempted use of Monitor mode in Non-secure state.
    if !IsSecure() then assert mode != M32_Monitor;
    assert !BadMode(mode);

    if mode == M32_Monitor then
        if n == 13 then
            return SP_mon;
        elsif n == 14 then
            return LR_mon;
        else
            return _R[n];
    else
        return _R[LookUpRIndex(n, mode)];

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
        else
            _R[n] = value;
    else
        _R[LookUpRIndex(n, mode)] = value;
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

__RAM(32) __Memory;

// Fake functions used for globals collection.
// To be overridden by the translator

Mem_Internal_Set(bits(32) address, integer size, bits(8*size) value)
  __Memory[address] = bits(8) UNKNOWN;
  return;

bits(8*size) Mem_Internal_Get(bits(32) address, integer size)
  bits(8) somebit = __Memory[address];
  return bits(8*size) UNKNOWN;


// Overriding memory access functions to short-circuit address translation

bits(8*size) MemA[bits(32) address, integer size]
    return Mem_Internal_Get(address, size);

MemA[bits(32) address, integer size] = bits(8*size) value
    Mem_Internal_Set(address, size, value);
    return;

bits(8*size) MemU_unpriv[bits(32) address, integer size]
    return Mem_Internal_Get(address, size);

MemU_unpriv[bits(32) address, integer size] = bits(8*size) value
    Mem_Internal_Set(address, size, value);
    return;

bits(8*size) MemU[bits(32) address, integer size]
    return Mem_Internal_Get(address, size);

MemU[bits(32) address, integer size] = bits(8*size) value
    Mem_Internal_Set(address, size, value);
    return;

bits(8*size) MemO[bits(32) address, integer size]
    return Mem_Internal_Get(address, size);

MemO[bits(32) address, integer size] = bits(8*size) value
    Mem_Internal_Set(address, size, value);
    return;

// Since IsExclusiveGlobal is stubbed to be FALSE, this will always be FALSE
boolean AArch32.ExclusiveMonitorsPass(bits(32) address, integer size)
    return FALSE;

// Since MarkExclusiveVA simply asserts FALSE, this will always assert FALSE
AArch32.SetExclusiveMonitors(bits(32) address, integer size)
    assert FALSE;
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

DCPSInstruction(bits(2) target_el)
    return;

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

