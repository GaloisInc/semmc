bits(N) Zeros()
    return Zeros(N);

bits(N) ZeroExtend(bits(M) val)
    return ZeroExtend(val, N);

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