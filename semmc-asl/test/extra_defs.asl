bits(N) Zeros()
    return Zeros(N);

bits(N) ZeroExtend(bits(M) val)
    return ZeroExtend(val, N);

bits(N*M) Replicate(bits(N) bv)
    return Replicate(bv, M);

constant integer LOG2_TAG_GRANULE=4;
constant integer TAG_GRANULE=2 ^ LOG2_TAG_GRANULE;

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
    reg = bits(64) UNKNOWN;
    return reg;

ReservedEncoding()
    return;

boolean IsPhysicalSErrorPending()
    ret = boolean UNKNOWN;
    return ret;

integer MinInt(integer i1, integer i2)
    if i1 < i2 then
        return i1;
    else
        return i2;