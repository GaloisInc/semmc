{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functions for converting between What4 and Crucible types.
module SemMC.ASL.Types
  ( ToBaseType
  , ToBaseTypes
  , ToBaseTypesList
  , ToCrucibleTypes
  , ConstVal(..)
  , UserType(..)
  , LabeledValue(..)
  , TypeEnvir
  , userTypeRepr
  , toBaseType
  , toBaseTypes
  , toCrucTypes
  , baseCrucProof
  , projectLabel
  , projectValue
  , emptyTypeEnvir
  , pushFreshTypeEnvir
  , popTypeEnvir
  , lookupTypeEnvir
  , insertTypeEnvir
  , flatTypeEnvir
  , fromListTypeEnvir
  , mkFinalFunctionName
  ) where

import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.TraversableFC as FC
import           Numeric.Natural ( Natural )
import qualified Data.Text as T
import qualified Lang.Crucible.Types as CT
import What4.BaseTypes as WT
import           Data.Parameterized.Classes
import qualified Data.BitVector.Sized as BVS
import qualified Language.ASL.Syntax as AS
import qualified Data.Map as Map
import qualified Data.List as List

type family ToBaseType (ctp :: CT.CrucibleType) :: WT.BaseType where
  ToBaseType (CT.BaseToType bt) = bt

type family ToBaseTypes (ctps :: CT.Ctx CT.CrucibleType) :: CT.Ctx WT.BaseType where
  ToBaseTypes CT.EmptyCtx = CT.EmptyCtx
  ToBaseTypes (tps CT.::> tp) = ToBaseTypes tps CT.::> ToBaseType tp

toBaseType :: CT.TypeRepr ctp -> WT.BaseTypeRepr (ToBaseType ctp)
toBaseType repr = case CT.asBaseType repr of
  CT.NotBaseType -> error "Illegal crucible type"
  CT.AsBaseType brepr -> brepr

toBaseTypes :: Ctx.Assignment CT.TypeRepr ctps -> Ctx.Assignment WT.BaseTypeRepr (ToBaseTypes ctps)
toBaseTypes Ctx.Empty = Ctx.Empty
toBaseTypes (reprs Ctx.:> repr) = toBaseTypes reprs Ctx.:> toBaseType repr

type family ToCrucibleTypes (wtps :: CT.Ctx WT.BaseType) :: CT.Ctx CT.CrucibleType where
  ToCrucibleTypes CT.EmptyCtx = CT.EmptyCtx
  ToCrucibleTypes (tps CT.::> tp) = ToCrucibleTypes tps CT.::> CT.BaseToType tp

type family ToBaseTypesList (ctps :: CT.Ctx CT.CrucibleType) :: [WT.BaseType] where
  ToBaseTypesList CT.EmptyCtx = '[]
  ToBaseTypesList (tps CT.::> tp) = ToBaseType tp ': ToBaseTypesList tps

type family ToCrucTypes (wtps :: CT.Ctx WT.BaseType) :: CT.Ctx CT.CrucibleType where
  ToCrucTypes CT.EmptyCtx = CT.EmptyCtx
  ToCrucTypes (wtps CT.::> wtp) = ToCrucTypes wtps CT.::> CT.BaseToType wtp

toCrucTypes :: Ctx.Assignment WT.BaseTypeRepr wtps -> Ctx.Assignment CT.TypeRepr (ToCrucTypes wtps)
toCrucTypes Ctx.Empty = Ctx.Empty
toCrucTypes (wReprs Ctx.:> wRepr) = toCrucTypes wReprs Ctx.:> CT.baseToType wRepr

baseCrucProof :: Ctx.Assignment WT.BaseTypeRepr wtps -> (ToBaseTypes (ToCrucTypes wtps) :~: wtps)
baseCrucProof Ctx.Empty = Refl
baseCrucProof (wtps Ctx.:> _) = case baseCrucProof wtps of
  Refl -> Refl


data LabeledValue a b tp = LabeledValue a (b tp)

instance (Eq a, TestEquality b) => TestEquality (LabeledValue a b) where
  LabeledValue a b `testEquality` LabeledValue a' b' =
    case b `testEquality` b' of
      Just Refl -> case a == a' of
        True -> Just Refl
        False -> Nothing
      Nothing -> Nothing

projectValue :: LabeledValue a b tp -> b tp
projectValue (LabeledValue _ v) = v

projectLabel :: LabeledValue a b tp -> a
projectLabel (LabeledValue l _) = l

instance FC.FunctorFC (LabeledValue a) where
  fmapFC f (LabeledValue a b) = LabeledValue a (f b)

instance FC.FoldableFC (LabeledValue a) where
  foldrFC f s (LabeledValue _ b) = f b s

instance FC.TraversableFC (LabeledValue a) where
  traverseFC f (LabeledValue a b) = LabeledValue a <$> f b

instance (Show a, ShowF b) => ShowF (LabeledValue a b) where
  showF (LabeledValue l v) = concat [ "LabeledValue ", show l, " ", showF v ]

instance (Show a, ShowF b) => Show (LabeledValue a b tp) where
  show (LabeledValue l v) = concat [ "LabeledValue ", show l, " ", showF v ]

type family BaseLitType (tp :: WT.BaseType) :: * where
  BaseLitType WT.BaseIntegerType = Integer
  BaseLitType WT.BaseBoolType = Bool
  BaseLitType (WT.BaseBVType w) = BVS.BitVector w
  
data ConstVal tp =
  ConstVal (WT.BaseTypeRepr tp) (BaseLitType tp)

instance Show (ConstVal tp) where
  show (ConstVal WT.BaseIntegerRepr l) = show l
  show (ConstVal WT.BaseBoolRepr l) = show l
  show (ConstVal (WT.BaseBVRepr _) l) = show l
  show _ = "??"

instance ShowF ConstVal

data UserType (tp :: WT.BaseType) where
  UserEnum :: Natural -> UserType WT.BaseIntegerType
  UserStruct :: Ctx.Assignment (LabeledValue (T.Text, Maybe (Some UserType)) WT.BaseTypeRepr) tps ->
                UserType (WT.BaseStructType tps)

deriving instance Show (UserType tp)

instance ShowF UserType where

userTypeRepr :: UserType tp -> WT.BaseTypeRepr tp
userTypeRepr ut =
  case ut of
    UserEnum _ -> WT.BaseIntegerRepr
    UserStruct tps -> WT.BaseStructRepr (FC.fmapFC projectValue tps)


data TypeEnvir = TypeEnvir { unTypeEnvir :: [Map.Map T.Text Integer] }
  deriving Show

emptyTypeEnvir :: TypeEnvir
emptyTypeEnvir = TypeEnvir [Map.empty]

pushFreshTypeEnvir :: TypeEnvir -> TypeEnvir
pushFreshTypeEnvir (TypeEnvir e) = TypeEnvir (Map.empty : e)

popTypeEnvir :: TypeEnvir -> TypeEnvir
popTypeEnvir (TypeEnvir (_ : e)) = TypeEnvir e
popTypeEnvir _ = error "Cannot pop empty type environment"

lookupTypeEnvir :: T.Text -> TypeEnvir -> Maybe Integer
lookupTypeEnvir nm (TypeEnvir (e : es)) =
  case Map.lookup nm e of
    Just i -> Just i
    _ -> lookupTypeEnvir nm (TypeEnvir es)
lookupTypeEnvir nm _ = Nothing


insertTypeEnvir :: T.Text -> Integer -> TypeEnvir -> TypeEnvir
insertTypeEnvir nm i env@(TypeEnvir (e : es)) =
  case lookupTypeEnvir nm env of
    Just i' -> if i == i' then env
               else error $ "Attempted to assign value: " <> show i <>
                    " to " <> show nm <> " in enviroment: " <> show env
    Nothing -> TypeEnvir (Map.insert nm i e : es)

flatTypeEnvir :: TypeEnvir -> [(T.Text, Integer)]
flatTypeEnvir (TypeEnvir (e : es)) = Map.assocs e ++ flatTypeEnvir (TypeEnvir es)
flatTypeEnvir (TypeEnvir _) = []

mkFinalFunctionName :: TypeEnvir -> T.Text ->  T.Text
mkFinalFunctionName dargs nm = T.concat $ [nm] ++ map (\(nm,i) -> nm <> "_" <> T.pack (show i)) (flatTypeEnvir dargs)

fromListTypeEnvir :: [(T.Text, Integer)] -> TypeEnvir
fromListTypeEnvir = List.foldr (\(nm,i) -> insertTypeEnvir nm i) emptyTypeEnvir
