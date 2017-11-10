{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
module SemMC.Architecture.PPC.Base.Memory (
  manualMemory
  ) where

import Prelude hiding ( concat )
import Control.Monad ( when )
import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core
import SemMC.Architecture.PPC.Base.Core.Memory

manualMemory :: (?bitSize :: BitSize) => SemM 'Top ()
manualMemory = do
  defineLoads
  defineStores

defineStores :: (?bitSize :: BitSize) => SemM 'Top ()
defineStores = do
  defineOpcodeWithIP "STB" $ do
    comment "Store Byte (D-form)"
    store 1
  defineOpcodeWithIP "STBU" $ do
    comment "Store Byte with Update (D-form)"
    storeWithUpdate 1
  defineOpcodeWithIP "STBX" $ do
    comment "Store Byte Indexed (X-form)"
    storeIndexed 1
  defineOpcodeWithIP "STBUX" $ do
    comment "Store Byte with Update Indexed (X-form)"
    storeWithUpdateIndexed 1

  defineOpcodeWithIP "STH" $ do
    comment "Store Halfword (D-form)"
    store 2
  defineOpcodeWithIP "STHU" $ do
    comment "Store Halfword with Update (D-form)"
    storeWithUpdate 2
  defineOpcodeWithIP "STHX" $ do
    comment "Store Halfword Indexed (X-form)"
    storeIndexed 2
  defineOpcodeWithIP "STHUX" $ do
    comment "Store Halfword with Update Indexed (X-form)"
    storeWithUpdateIndexed 2

  defineOpcodeWithIP "STW" $ do
    comment "Store Word (D-form)"
    store 4
  defineOpcodeWithIP "STWU" $ do
    comment "Store Word with Update (D-form)"
    storeWithUpdate 4
  defineOpcodeWithIP "STWX" $ do
    comment "Store Word Indexed (X-form)"
    storeIndexed 4
  defineOpcodeWithIP "STWUX" $ do
    comment "Store Word with Update Indexed (X-form)"
    storeWithUpdateIndexed 4

  when (?bitSize == Size64) $ do
    defineOpcodeWithIP "STD"$ do
      comment "Store Doubleword (DS-form)"
      storeDS 8
    defineOpcodeWithIP "STDU" $ do
      comment "Store Doubleword with Update (DS-form)"
      storeWithUpdateDS 8
    defineOpcodeWithIP "STDX" $ do
      comment "Store Doubleword Indexed (X-form)"
      storeIndexed 8
    defineOpcodeWithIP "STDUX" $ do
      comment "Store Doubleword with Update Indexed (X-form)"
      storeWithUpdateIndexed 8

defineLoads :: (?bitSize :: BitSize) => SemM 'Top ()
defineLoads = do
  defineOpcodeWithIP "LBZ" $ do
    comment "Load Byte and Zero (D-form)"
    loadAndExtend 1 zext
  defineOpcodeWithIP "LBZU" $ do
    comment "Load Byte and Zero with Update (D-form)"
    loadAndUpdate 1 zext
  defineOpcodeWithIP "LBZX" $ do
    comment "Load Byte and Zero Indexed (X-form)"
    loadIndexed 1 zext
  defineOpcodeWithIP "LBZUX" $ do
    comment "Load Byte and Zero with Update Indexed (X-form)"
    loadAndExtendWithUpdateIndexed 1 zext

  defineOpcodeWithIP "LHZ" $ do
    comment "Load Halfword and Zero (D-form)"
    loadAndExtend 2 zext
  defineOpcodeWithIP "LHZU" $ do
    comment "Load Halfword and Zero with Update (D-form)"
    loadAndUpdate 2 zext
  defineOpcodeWithIP "LHZX" $ do
    comment "Load Halfword and Zero Indexed (X-form)"
    loadIndexed 2 zext
  defineOpcodeWithIP "LHZUX" $ do
    comment "Load Halfword and Zero with Update Indexed (X-form)"
    loadAndExtendWithUpdateIndexed 2 zext

  defineOpcodeWithIP "LHAU" $ do
    comment "Load Halfword Algebraic with Update (D-form)"
    loadAndUpdate 2 sext
  defineOpcodeWithIP "LHA" $ do
    comment "Load Halfword Algebraic (D-form)"
    loadAndExtend 2 sext
  defineOpcodeWithIP "LHAX" $ do
    comment "Load Halfword Algebraic Indexed (X-form)"
    loadIndexed 2 sext
  defineOpcodeWithIP "LHAUX" $ do
    comment "Load Halfword Algebraic with Update Indexed (X-form)"
    loadAndExtendWithUpdateIndexed 2 sext

  defineOpcodeWithIP "LWZ" $ do
    comment "Load Word and Zero (D-form)"
    loadAndExtend 4 zext
  defineOpcodeWithIP "LWZU" $ do
    comment "Load Word and Zero with Update (D-form)"
    loadAndUpdate 4 zext
  defineOpcodeWithIP "LWZX" $ do
    comment "Load Word and Zero Indexed (X-form)"
    loadIndexed 4 zext
  defineOpcodeWithIP "LWZUX" $ do
    comment "Load Word and Zero with Update Indexed (X-form)"
    loadAndExtendWithUpdateIndexed 4 zext

  defineOpcodeWithIP "LWA" $ do
    comment "Load Word Algebraic (DS-form)"
    loadAndExtendDS 4 sext
  defineOpcodeWithIP "LWAX" $ do
    comment "Load Word Algebraic Indexed (X-form)"
    loadIndexed 4 sext
  defineOpcodeWithIP "LWAUX" $ do
    comment "Load Word Algebraic with Update Indexed (X-form)"
    loadAndExtendWithUpdateIndexed 4 sext

  -- The 64 bit variants never need extension, so we use id for the extension function.
  when (?bitSize == Size64) $ do
    defineOpcodeWithIP "LD" $ do
      comment "Load Doubleword (DS-form)"
      loadAndExtendDS 8 id
    defineOpcodeWithIP "LDU" $ do
      comment "Load Doubleword with Update (DS-form)"
      loadAndUpdateDS 8 id
    defineOpcodeWithIP "LDX" $ do
      comment "Load Doubleword Indexed (X-form)"
      loadIndexed 8 id
    defineOpcodeWithIP "LDUX" $ do
      comment "Load Doubleword and Update Indexed (X-form)"
      loadAndExtendWithUpdateIndexed 8 id

