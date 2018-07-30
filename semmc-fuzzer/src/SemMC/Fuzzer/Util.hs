{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module SemMC.Fuzzer.Util
  ( stateDiff
  , statePairs
  , makePlain
  , showBS
  )
where

import qualified Data.Set as S
import           Data.Maybe (catMaybes)

import qualified Data.ByteString as BS
import           Data.Parameterized.Some (Some(..))
import qualified Data.Parameterized.Map as MapF
import           Data.Word ( Word8 )
import qualified Data.Word.Indexed as W
import           Numeric ( showHex )

import qualified SemMC.Formula as F
import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.View as V
import qualified SemMC.Architecture.Value as V
import           SemMC.Synthesis.Template ( BaseSet, TemplatedArch
                                          , unTemplate
                                          )

statePairs :: (A.Architecture arch, MapF.ShowF (A.Location arch))
          => proxy arch
          -> V.ConcreteState arch
          -> [(String, String)]
statePairs _ s = [ (MapF.showF k, showValue v) | MapF.Pair k v <- MapF.toList s ]

showValue :: V.Value tp -> String
showValue val =
    case val of
        V.ValueBV v -> show $ W.unW v
        V.ValueMem bs -> showBS bs

showBS :: BS.ByteString -> String
showBS bs = "0x" ++ (concat $ showByte <$> (BS.unpack bs))

showByte :: Word8 -> String
showByte b =
    let s = showHex b ""
    in (if length s == 1 then "0" else "") ++ s

stateDiff :: (A.Architecture arch)
          => proxy arch
          -> V.ConcreteState arch
          -> V.ConcreteState arch
          -> [(Some (A.Location arch), (Maybe (Some V.Value), Maybe (Some V.Value)))]
stateDiff _ a b =
    let aKeys = S.fromList $ MapF.keys a
        bKeys = S.fromList $ MapF.keys b
        allKeys = S.union aKeys bKeys
        pairs = [ (k,) <$>
                  case k of
                    Some v ->
                      let aVal = MapF.lookup v a
                          bVal = MapF.lookup v b
                      in if aVal == bVal
                         then Nothing
                         else Just (Some <$> aVal, Some <$> bVal)
                | k <- S.toList allKeys
                ]
    in catMaybes pairs

makePlain :: forall arch sym
           . (MapF.OrdF (A.Opcode arch (A.Operand arch)),
              MapF.OrdF (A.Location arch))
          => BaseSet sym arch
          -> MapF.MapF (A.Opcode arch (A.Operand arch)) (F.ParameterizedFormula sym arch)
makePlain = MapF.foldrWithKey f MapF.empty
  where f :: forall sh
           . A.Opcode arch (A.Operand arch) sh
          -> F.ParameterizedFormula sym (TemplatedArch arch) sh
          -> MapF.MapF (A.Opcode arch (A.Operand arch)) (F.ParameterizedFormula sym arch)
          -> MapF.MapF (A.Opcode arch (A.Operand arch)) (F.ParameterizedFormula sym arch)
        f op pf = MapF.insert op (unTemplate pf)
