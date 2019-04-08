{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module SemMC.ASL.Extension (
    ASLExt
  , ASLApp(..)
  , ASLStmt
  , aslExtImpl
  ) where

import qualified Control.Exception as X
import           Control.Monad ( guard )
import           Data.Functor.Product ( Product(..) )
import qualified Data.Map as Map
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.TraversableFC as FC
import qualified Data.Text as T
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.CFG.Extension as CCExt
import qualified Lang.Crucible.CFG.Extension.Safety as CCES
import qualified Lang.Crucible.Simulator as CS
import qualified Lang.Crucible.Simulator.Evaluation as CSE
import qualified Lang.Crucible.Types as CT
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified What4.BaseTypes as WT
import qualified What4.Interface as WI
import qualified What4.Symbol as WS

import           SemMC.ASL.Exceptions ( TranslationException(..) )
import           SemMC.ASL.Signature ( SomeSignature )

-- NOTE: Translate calls (both expr and stmt) as What4 uninterpreted functions
--
-- Actually, might even need an extension to represent this properly, since we don't want an actual
-- call.  The return value of the EvalAppFunc is a RegValue (which can be a SymExpr -- a What4 UF).

-- | A language extension for the ASL translation
--
-- Currently, it has no parameters, but we might need to add one if there are arch-specific
-- extensions required later.
--
-- The main reason we need this is in translating calls, which we need to keep as uninterpreted.  It
-- doesn't seem like we can represent that directly in Crucible; this allows us to translate
-- uninterpreted functions into UFs directly in What4.
data ASLExt

data ASLApp f tp where
  UF :: T.Text -> WT.BaseTypeRepr rtp -> Ctx.Assignment CT.TypeRepr tps -> Ctx.Assignment f tps -> ASLApp f (CT.BaseToType rtp)

instance FC.FunctorFC ASLApp where
  fmapFC f a =
    case a of
      UF name trep argReps vals -> UF name trep argReps (FC.fmapFC f vals)

instance FC.FoldableFC ASLApp where
  foldrFC f seed a =
    case a of
      UF _ _ _ vals -> FC.foldrFC f seed vals

instance FC.TraversableFC ASLApp where
  traverseFC f a =
    case a of
      UF name trep argReps vals -> UF name trep argReps <$> FC.traverseFC f vals

instance CCExt.TypeApp ASLApp where
  appType a =
    case a of
      UF _ trep _ _ -> CT.baseToType trep

instance CCExt.PrettyApp ASLApp where
  ppApp pp a =
    case a of
      UF name trep argReps vals ->
        PP.hsep [ PP.text (T.unpack name)
               , PP.text (show trep)
               , PP.brackets (PP.cat (PP.punctuate PP.comma (FC.toListFC (PP.text . showF) argReps)))
               , PP.brackets (PP.cat (PP.punctuate PP.comma (FC.toListFC pp vals)))
               ]

instance FC.TestEqualityFC ASLApp where
  testEqualityFC testFC a1 a2 =
    case (a1, a2) of
      (UF n1 r1 rs1 vs1, UF n2 r2 rs2 vs2) -> do
        guard (n1 == n2)
        Refl <- testEquality r1 r2
        Refl <- testEquality rs1 rs2
        Refl <- FC.testEqualityFC testFC vs1 vs2
        return Refl

instance FC.OrdFC ASLApp where
  compareFC compareTerm a1 a2 =
    case (a1, a2) of
      (UF n1 r1 rs1 vs1, UF n2 r2 rs2 vs2) ->
        case compare n1 n2 of
          LT -> LTF
          GT -> GTF
          EQ -> case compareF r1 r2 of
            LTF -> LTF
            GTF -> GTF
            EQF -> case compareF rs1 rs2 of
              LTF -> LTF
              GTF -> GTF
              EQF -> case FC.compareFC compareTerm vs1 vs2 of
                LTF -> LTF
                GTF -> GTF
                EQF -> EQF

data ASLStmt f tp where

instance FC.FunctorFC ASLStmt where
  fmapFC _ = \case

instance FC.FoldableFC ASLStmt where
  foldrFC _ _ = \case

instance FC.TraversableFC ASLStmt where
  traverseFC _ = \case

instance CCExt.TypeApp ASLStmt where
  appType = \case

instance CCExt.PrettyApp ASLStmt where
  ppApp _ = \case

type instance CCES.AssertionClassifier ASLExt = CCES.NoAssertionClassifier

instance CCES.HasStructuredAssertions ASLExt where
  explain _ = \case
  toPredicate _ _ = \case


-- | The ASLExt app evaluator
--
-- NOTE: Right now, this generates a fresh uninterpreted function for each call.  That should be
-- fine, as we don't need to test for equality between the results of any calls.
aslAppEvalFunc :: forall sym
                . (CB.IsSymInterface sym)
               => (Map.Map T.Text SomeSignature)
               -- ^ A mapping from function names to signatures, which we'll use to generate (and check) call signatures
               -> sym
               -> CS.IntrinsicTypes sym
               -> (Int -> String -> IO ())
               -> CSE.EvalAppFunc sym ASLApp
aslAppEvalFunc sigs sym _ _ = \evalApp app ->
  case app of
    UF name trep argTps args ->
      case WS.userSymbol (T.unpack name) of
        Left _err -> X.throw (InvalidFunctionName name)
        Right funcSymbol -> do
          Some baseTypesArgs <- extractBase name (\v -> CS.RV <$> evalApp v) argTps args (Some Ctx.empty)
          let baseTps = FC.fmapFC fstFC baseTypesArgs
          let baseArgs = FC.fmapFC (unSE @sym . sndFC) baseTypesArgs
          symFn <- WI.freshTotalUninterpFn sym funcSymbol baseTps trep
          WI.applySymFn sym symFn baseArgs

-- | A wrapper around 'WI.SymExpr' because it is a type family and not injective
data SymExpr' sym tp = SE { unSE :: WI.SymExpr sym tp }

fstFC :: Product f g a -> f a
fstFC (Pair f _) = f

sndFC :: Product f g a -> g a
sndFC (Pair _ g) = g

extractBase :: T.Text
            -> (forall tp1 . f tp1 -> IO (CS.RegValue' sym tp1))
            -> Ctx.Assignment CT.TypeRepr tps
            -> Ctx.Assignment f tps
            -> Some (Ctx.Assignment (Product WT.BaseTypeRepr (SymExpr' sym)))
            -> IO (Some (Ctx.Assignment (Product WT.BaseTypeRepr (SymExpr' sym))))
extractBase fname evalExpr tps vals (Some acc) = do
  case (Ctx.viewAssign tps, Ctx.viewAssign vals) of
    (Ctx.AssignEmpty, Ctx.AssignEmpty) -> return (Some acc)
    (Ctx.AssignExtend restReps rep, Ctx.AssignExtend restVals val) -> do
      case CT.asBaseType rep of
        CT.NotBaseType -> X.throwIO (ExpectedBaseTypeArgument fname rep)
        CT.AsBaseType btr -> do
          CS.RV se <- evalExpr val
          let acc' = Ctx.extend acc (Pair btr (SE se))
          extractBase fname evalExpr restReps restVals (Some acc')


type instance CCExt.ExprExtension ASLExt = ASLApp
type instance CCExt.StmtExtension ASLExt = ASLStmt

instance CCExt.IsSyntaxExtension ASLExt

aslExtImpl :: Map.Map T.Text SomeSignature -> CS.ExtensionImpl p sym ASLExt
aslExtImpl sigs =
  CS.ExtensionImpl { CS.extensionEval = aslAppEvalFunc sigs
                   , CS.extensionExec = \case
                   }
