{-# LANGUAGE DataKinds #-}
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
  , ASLStmt(..)
  , aslExtImpl
  ) where

import qualified Control.Exception as X
import           Control.Lens ( (^.), (&), (.~) )
import           Control.Monad ( guard )
import           Data.Functor.Product ( Product(..) )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.TraversableFC as FC
import           Data.Proxy ( Proxy(..) )
import qualified Data.Text as T
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.CFG.Extension as CCExt
import qualified Lang.Crucible.CFG.Extension.Safety as CCES
import qualified Lang.Crucible.Simulator as CS
import qualified Lang.Crucible.Simulator.ExecutionTree as CSET
import qualified Lang.Crucible.Simulator.Evaluation as CSE
import qualified Lang.Crucible.Simulator.GlobalState as CSG
import qualified Lang.Crucible.Simulator.RegValue as CSR
import qualified Lang.Crucible.Types as CT
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified What4.BaseTypes as WT
import qualified What4.Interface as WI
import qualified What4.Symbol as WS

import           SemMC.ASL.Exceptions ( TranslationException(..) )
import           SemMC.ASL.Signature ( BaseGlobalVar(..) )

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
data ASLExt arch

data ASLApp f tp where
  UF :: T.Text
     -> WT.BaseTypeRepr rtp
     -> Ctx.Assignment CT.TypeRepr tps
     -> Ctx.Assignment f tps
     -> ASLApp f (CT.BaseToType rtp)
  GetBaseStruct :: CT.TypeRepr (CT.SymbolicStructType ctx)
                -> Ctx.Index ctx tp
                -> f (CT.SymbolicStructType ctx)
                -> ASLApp f (CT.BaseToType tp)

-- | The statement extension type
--
-- These forms must be statements because only statements have access to the
-- solver state
data ASLStmt arch f tp where
  GetRegState :: Ctx.Assignment WT.BaseTypeRepr regs
              -> Ctx.Assignment BaseGlobalVar regs
              -> ASLStmt arch f (CT.SymbolicStructType regs)
  SetRegState :: Ctx.Assignment BaseGlobalVar regs
              -> f (CT.SymbolicStructType regs)
              -> ASLStmt arch f CT.UnitType


-- | The ASLExt app evaluator
--
-- NOTE: Right now, this generates a fresh uninterpreted function for each call.  That should be
-- fine, as we don't need to test for equality between the results of any calls.
aslAppEvalFunc :: forall sym arch proxy
                . (CB.IsSymInterface sym)
               => proxy arch
               -> sym
               -> CS.IntrinsicTypes sym
               -> (Int -> String -> IO ())
               -> CSE.EvalAppFunc sym ASLApp
aslAppEvalFunc _ sym _ _ = \evalApp app ->
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
    GetBaseStruct _srep idx term -> do
      rv <- evalApp term
      WI.structField sym rv idx

aslStmtEvalFunc :: forall arch sym tp p rtp blocks r ctx
                 . ASLStmt arch (CS.RegEntry sym) tp
                -> CS.CrucibleState p sym (ASLExt arch) rtp blocks r ctx
                -> IO (CSR.RegValue sym tp, CS.CrucibleState p sym (ASLExt arch) rtp blocks r ctx)
aslStmtEvalFunc stmt ctx =
  case stmt of
    GetRegState _reps bvs -> CS.ctxSolverProof (ctx ^. CS.stateContext) $ do
      let sym = ctx ^. CS.stateContext . CS.ctxSymInterface
      let fr = ctx ^. (CSET.stateTree . CSET.actFrame)
      let globals = fr ^. CSET.gpGlobals
      gvals <- FC.traverseFC (readBaseGlobal globals) bvs
      struct <- WI.mkStruct sym gvals
      return (struct, ctx)
    SetRegState (bvs :: Ctx.Assignment BaseGlobalVar regs) vals -> CS.ctxSolverProof (ctx ^. CS.stateContext) $ do
      let sym = ctx ^. CS.stateContext . CS.ctxSymInterface
      let fr = ctx ^. (CSET.stateTree . CSET.actFrame)
      let globals = fr ^. CSET.gpGlobals
      let updateGlobal :: forall tp' . IO (CSG.SymGlobalState sym) -> Ctx.Index regs tp' -> IO (CSG.SymGlobalState sym)
          updateGlobal mgs idx =
            case bvs Ctx.! idx of
              BaseGlobalVar gv -> do
                gs <- mgs
                let rv = CS.regValue vals
                val <- WI.structField sym rv idx
                return (CSG.insertGlobal gv val gs)
      globals' <- Ctx.forIndex (Ctx.size bvs) updateGlobal (pure globals)
      return ((), ctx & CSET.stateTree . CSET.actFrame . CSET.gpGlobals .~ globals')


readBaseGlobal :: (Monad m)
               => CS.SymGlobalState sym
               -> BaseGlobalVar tp
               -> m (WI.SymExpr sym tp)
readBaseGlobal gs (BaseGlobalVar gv) =
  case CSG.lookupGlobal gv gs of
    Nothing -> error ("Unbound global register: " ++ show gv)
    Just rv -> return rv

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


type instance CCExt.ExprExtension (ASLExt arch) = ASLApp
type instance CCExt.StmtExtension (ASLExt arch) = ASLStmt arch

instance CCExt.IsSyntaxExtension (ASLExt arch)

aslExtImpl :: forall arch p sym . CS.ExtensionImpl p sym (ASLExt arch)
aslExtImpl =
  CS.ExtensionImpl { CS.extensionEval = aslAppEvalFunc (Proxy @arch)
                   , CS.extensionExec = aslStmtEvalFunc
                   }


instance FC.FunctorFC (ASLStmt arch) where
  fmapFC f a =
    case a of
      GetRegState reps gvs -> GetRegState reps gvs
      SetRegState gvs s -> SetRegState gvs (f s)

instance FC.FoldableFC (ASLStmt arch) where
  foldrFC _f seed a =
    case a of
      GetRegState _ _ -> seed
      SetRegState _ _ -> seed

instance FC.TraversableFC (ASLStmt arch) where
  traverseFC f a =
    case a of
      GetRegState reps gvs -> pure (GetRegState reps gvs)
      SetRegState gvs s -> SetRegState gvs <$> f s

instance CCExt.TypeApp (ASLStmt arch) where
  appType a =
    case a of
      GetRegState reps _ -> CT.baseToType (WT.BaseStructRepr reps)
      SetRegState _ _ -> CT.UnitRepr

instance CCExt.PrettyApp (ASLStmt arch) where
  ppApp pp a =
    case a of
      GetRegState _reps regs ->
        PP.hsep [ PP.text "GetRegState"
                , PP.brackets (PP.cat (PP.punctuate PP.comma (FC.toListFC (PP.text . showF) regs)))
                ]
      SetRegState gvs vs ->
        PP.hsep [ PP.text "SetRegState"
                , PP.brackets (PP.cat (PP.punctuate PP.comma (FC.toListFC (PP.text . showF) gvs)))
                , PP.comma
                , PP.brackets (pp vs)
                ]

type instance CCES.AssertionClassifier (ASLExt arch) = CCES.NoAssertionClassifier

instance CCES.HasStructuredAssertions (ASLExt arch) where
  explain _ = \case
  toPredicate _ _ = \case


instance FC.FunctorFC ASLApp where
  fmapFC f a =
    case a of
      UF name trep argReps vals -> UF name trep argReps (FC.fmapFC f vals)
      GetBaseStruct rep i t -> GetBaseStruct rep i (f t)

instance FC.FoldableFC ASLApp where
  foldrFC f seed a =
    case a of
      UF _ _ _ vals -> FC.foldrFC f seed vals
      GetBaseStruct _ _ t -> f t seed

instance FC.TraversableFC ASLApp where
  traverseFC f a =
    case a of
      UF name trep argReps vals -> UF name trep argReps <$> FC.traverseFC f vals
      GetBaseStruct rep i t -> GetBaseStruct rep i <$> f t

instance CCExt.TypeApp ASLApp where
  appType a =
    case a of
      UF _ trep _ _ -> CT.baseToType trep
      GetBaseStruct (CT.SymbolicStructRepr reprs) i _ -> CT.baseToType (reprs Ctx.! i)

instance CCExt.PrettyApp ASLApp where
  ppApp pp a =
    case a of
      UF name trep argReps vals ->
        PP.hsep [ PP.text (T.unpack name)
               , PP.text (show trep)
               , PP.brackets (PP.cat (PP.punctuate PP.comma (FC.toListFC (PP.text . showF) argReps)))
               , PP.brackets (PP.cat (PP.punctuate PP.comma (FC.toListFC pp vals)))
               ]
      GetBaseStruct _r i t ->
        PP.hsep [ PP.text "GetBaseStruct"
                , PP.text (showF i)
                , pp t
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
      (GetBaseStruct r1 i1 t1, GetBaseStruct r2 i2 t2) -> do
        Refl <- testEquality r1 r2
        Refl <- testEquality i1 i2
        Refl <- testFC t1 t2
        return Refl
      _ -> Nothing

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
      (GetBaseStruct r1 i1 t1, GetBaseStruct r2 i2 t2) ->
        case compareF r1 r2 of
          LTF -> LTF
          GTF -> GTF
          EQF -> case compareF i1 i2 of
            LTF -> LTF
            GTF -> GTF
            EQF -> case compareTerm t1 t2 of
              LTF -> LTF
              GTF -> GTF
              EQF -> EQF
      (UF {}, _) -> LTF
      (GetBaseStruct {}, _) -> GTF
