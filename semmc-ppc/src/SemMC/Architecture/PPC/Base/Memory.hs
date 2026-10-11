{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
module SemMC.Architecture.PPC.Base.Memory (
  manualMemory
  ) where

import Prelude hiding ( concat )
import Control.Monad ( forM_, when )
import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core
import SemMC.Architecture.PPC.Base.Core.Memory

manualMemory :: (?bitSize :: BitSize) => SemM 'Top ()
manualMemory = do
  defineLoads
  defineStores

defineStores :: (?bitSize :: BitSize) => SemM 'Top ()
defineStores = do
  when (?bitSize == Size32) $ do
    defineOpcodeWithIP "STMW" $ do
      comment "Store Multiple Word (D-form)"
      dst <- param "dst" memri EMemRef
      rS <- param "rS" gprc naturalBV
      input dst
      input memory
      let start = "stmw_start" =: gprIndex (Loc rS)
      let rA = memriReg dst
      let disp = memriOffset 16 (Loc dst)
      let b = ite (isR0 (Loc rA)) (naturalLitBV 0) (Loc rA)
      let ea = "stmw_ea" =: bvadd b (sext disp)
      forM_ [0..31] (input . gprLoc)
      let storeSuffix first =
            foldl
              (\mem n ->
                let address =
                      bvadd ea (naturalLitBV (toInteger ((n - first) * 4)))
                in storeMem mem address 4 (Loc (gprLoc n)))
              (Loc memory)
              [first..31]
      let selectSuffix first fallback =
            ite
              (bveq start (LitBV 5 (toInteger first)))
              (storeSuffix first)
              fallback
      defLoc memory (foldr selectSuffix (Loc memory) [0..31])

    defineOpcodeWithIP "STSWI" $ do
      comment "Store String Word Immediate (X-form)"
      nB <- param "NB" u5imm (EBV 5)
      rA <- param "rA" gprc naturalBV
      rS <- param "rS" gprc naturalBV
      input rA
      input memory
      let ea = ite (isR0 (Loc rA)) (naturalLitBV 0) (Loc rA)
      let sourceRegs = [gprOffset word rS | word <- [0..7]]
      forM_ sourceRegs input
      let storeByte mem byte =
            let source = sourceRegs !! (byte `div` 4)
                byteInRegister = byte `mod` 4
                value =
                  extract
                    (byteInRegister * 8 + 7)
                    (byteInRegister * 8)
                    (Loc source)
            in storeMem
                 mem
                 (bvadd ea (naturalLitBV (toInteger byte)))
                 1
                 value
      let storeBytes count =
            foldl storeByte (Loc memory) [0..count - 1]
      let selectByteCount count fallback =
            ite
              (bveq (Loc nB) (LitBV 5 (toInteger count)))
              (storeBytes count)
              fallback
      defLoc memory (foldr selectByteCount (storeBytes 32) [1..31])

  defineOpcodeWithIP "STB" $ do
    comment "Store Byte (D-form)"
    store 1
  defineOpcodeWithIP "STBU" $ do
    comment "Store Byte with Update (D-form)"
    storeWithUpdate 1
  defineOpcodeWithIP "STBX" $ do
    comment "Store Byte Indexed (X-form)"
    storeIndexed 1 id
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
    storeIndexed 2 id
  defineOpcodeWithIP "STHUX" $ do
    comment "Store Halfword with Update Indexed (X-form)"
    storeWithUpdateIndexed 2
  defineOpcodeWithIP "STHBRX" $ do
    comment "Store Halfword Byte-Reverse Indexed (X-form)"
    storeIndexed 2 reverseBytes

  defineOpcodeWithIP "STW" $ do
    comment "Store Word (D-form)"
    store 4
  defineOpcodeWithIP "STWU" $ do
    comment "Store Word with Update (D-form)"
    storeWithUpdate 4
  defineOpcodeWithIP "STWX" $ do
    comment "Store Word Indexed (X-form)"
    storeIndexed 4 id
  defineOpcodeWithIP "STWUX" $ do
    comment "Store Word with Update Indexed (X-form)"
    storeWithUpdateIndexed 4
  defineOpcodeWithIP "STWBRX" $ do
    comment "Store Word Byte-Reverse Indexed (X-form)"
    storeIndexed 4 reverseBytes

  when (?bitSize == Size64) $ do
    defineOpcodeWithIP "STD"$ do
      comment "Store Doubleword (DS-form)"
      storeDS 8
    defineOpcodeWithIP "STDU" $ do
      comment "Store Doubleword with Update (DS-form)"
      storeWithUpdateDS 8
    defineOpcodeWithIP "STDX" $ do
      comment "Store Doubleword Indexed (X-form)"
      storeIndexed 8 id
    defineOpcodeWithIP "STDUX" $ do
      comment "Store Doubleword with Update Indexed (X-form)"
      storeWithUpdateIndexed 8
    defineOpcodeWithIP "STDBRX" $ do
      comment "Store Doubleword Byte-Reverse Indexed (X-form)"
      storeIndexed 8 reverseBytes

defineLoads :: (?bitSize :: BitSize) => SemM 'Top ()
defineLoads = do
  when (?bitSize == Size32) $ do
    defineOpcodeWithIP "LMW" $ do
      comment "Load Multiple Word (D-form)"
      rD <- param "rD" gprc naturalBV
      src <- param "src" memri EMemRef
      input src
      input memory
      let start = "lmw_start" =: gprIndex (Loc rD)
      let rA = memriReg src
      let disp = memriOffset 16 (Loc src)
      let b = ite (isR0 (Loc rA)) (naturalLitBV 0) (Loc rA)
      let ea = "lmw_ea" =: bvadd b (sext disp)
      forM_ [0..31] $ \n -> do
        let reg = gprLoc n
        input reg
        let active = bvule start (LitBV 5 (toInteger n))
        let wordOffset =
              bvmul
                (zext (bvsub (LitBV 5 (toInteger n)) start))
                (naturalLitBV 4)
        defLoc reg
          (ite active
            (readMem (Loc memory) (bvadd ea wordOffset) 4)
            (Loc reg))

    defineOpcodeWithIP "LSWI" $ do
      comment "Load String Word Immediate (X-form)"
      rT <- param "rT" gprc naturalBV
      nB <- param "NB" u5imm (EBV 5)
      rA <- param "rA" gprc naturalBV
      input rA
      input memory
      let ea = ite (isR0 (Loc rA)) (naturalLitBV 0) (Loc rA)
      let byteCount =
            "lswi_byte_count" =:
              ite
                (bveq (Loc nB) (LitBV 5 0))
                (naturalLitBV 32)
                (zext (Loc nB))
      forM_ [0..7] $ \word -> do
        let target = gprOffset word rT
        input target
        let loadByte byte =
              let offset = word * 4 + byte
                  active =
                    bvult (naturalLitBV (toInteger offset)) byteCount
              in ite
                   active
                   (readMem
                     (Loc memory)
                     (bvadd ea (naturalLitBV (toInteger offset)))
                     1)
                   (LitBV 8 0)
        let value = foldl1 concat [loadByte byte | byte <- [0..3]]
        let active =
              bvult (naturalLitBV (toInteger (word * 4))) byteCount
        defLoc target (ite active value (Loc target))

  defineOpcodeWithIP "LBZ" $ do
    comment "Load Byte and Zero (D-form)"
    loadAndExtend 1 zext
  defineOpcodeWithIP "LBZU" $ do
    comment "Load Byte and Zero with Update (D-form)"
    loadAndUpdate 1 zext
  defineOpcodeWithIP "LBZX" $ do
    comment "Load Byte and Zero Indexed (X-form)"
    loadIndexed 1 id zext
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
    loadIndexed 2 id zext
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
    loadIndexed 2 id sext
  defineOpcodeWithIP "LHAUX" $ do
    comment "Load Halfword Algebraic with Update Indexed (X-form)"
    loadAndExtendWithUpdateIndexed 2 sext
  defineOpcodeWithIP "LHBRX" $ do
    comment "Load Halfword Byte-Reverse Indexed (X-form)"
    loadIndexed 2 reverseBytes zext

  defineOpcodeWithIP "LWZ" $ do
    comment "Load Word and Zero (D-form)"
    loadAndExtend 4 zext
  defineOpcodeWithIP "LWZU" $ do
    comment "Load Word and Zero with Update (D-form)"
    loadAndUpdate 4 zext
  defineOpcodeWithIP "LWZX" $ do
    comment "Load Word and Zero Indexed (X-form)"
    loadIndexed 4 id zext
  defineOpcodeWithIP "LWZUX" $ do
    comment "Load Word and Zero with Update Indexed (X-form)"
    loadAndExtendWithUpdateIndexed 4 zext
  defineOpcodeWithIP "LWBRX" $ do
    comment "Load Word Byte-Reverse Indexed (X-form)"
    loadIndexed 4 reverseBytes zext

  defineOpcodeWithIP "LWA" $ do
    comment "Load Word Algebraic (DS-form)"
    loadAndExtendDS 4 sext
  defineOpcodeWithIP "LWAX" $ do
    comment "Load Word Algebraic Indexed (X-form)"
    loadIndexed 4 id sext
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
      loadIndexed 8 id id
    defineOpcodeWithIP "LDUX" $ do
      comment "Load Doubleword and Update Indexed (X-form)"
      loadAndExtendWithUpdateIndexed 8 id
    defineOpcodeWithIP "LDBRX" $ do
      comment "Load Doubleword Byte-Reverse Indexed (X-form)"
      loadIndexed 8 reverseBytes zext

gprLoc :: (?bitSize :: BitSize) => Int -> Location 'TBV
gprLoc n =
  LiteralLoc Literal
    { lName = "r" ++ show n
    , lExprType = naturalBV
    }

