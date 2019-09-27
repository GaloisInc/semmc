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

// We can likely stub these out safely
(boolean,boolean) AArch32.BreakpointValueMatch(integer n, bits(32) vaddress, boolean linked_to)
  return (FALSE, FALSE);


boolean AArch64.BreakpointValueMatch(integer n, bits(64) vaddress, boolean linked_to)
  return FALSE;

boolean IsBlockDescriptorNTBitValid()
  return FALSE;