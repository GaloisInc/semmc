{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module SemMC.Architecture.PPC.OperandComponents (
  OperandComponents(..)
  ) where

import           Data.Parameterized.Classes ( ShowF )
import           GHC.TypeLits ( Symbol )

import           SemMC.Architecture.PPC.Location ( Location, ArchRegWidth )
import           What4.BaseTypes ( BaseBVType )
import qualified What4.Interface as S

data OperandComponents ppc sym (s :: Symbol) where
  OCMemri :: Maybe (Location ppc tp)
          -> S.SymExpr sym (BaseBVType (ArchRegWidth ppc))
          -> S.SymExpr sym (BaseBVType 16)
          -> OperandComponents ppc sym s
  OCMemrix :: Maybe (Location ppc tp)
           -> S.SymExpr sym (BaseBVType (ArchRegWidth ppc))
           -> S.SymExpr sym (BaseBVType 14)
           -> OperandComponents ppc sym s
  OCMemrr :: Maybe (Location ppc tp)
           -> S.SymExpr sym (BaseBVType (ArchRegWidth ppc))
           -> Location ppc tp
           -> S.SymExpr sym (BaseBVType (ArchRegWidth ppc))
           -> OperandComponents ppc sym s

instance (S.IsExpr (S.SymExpr sym)) => Show (OperandComponents ppc sym s) where
  show oc =
    case oc of
      OCMemri ml e1 e2 -> unwords [ "OCMemri"
                                  , show ml
                                  , show (S.printSymExpr e1)
                                  , show (S.printSymExpr e2)
                                  ]
      OCMemrix ml e1 e2 -> unwords [ "OCMemrix"
                                   , show ml
                                   , show (S.printSymExpr e1)
                                   , show (S.printSymExpr e2)
                                   ]
      OCMemrr ml e1 l e2 -> unwords [ "OCMemrr"
                                    , show ml
                                    , show (S.printSymExpr e1)
                                    , show l
                                    , show (S.printSymExpr e2)
                                    ]

instance (S.IsExpr (S.SymExpr sym)) => ShowF (OperandComponents ppc sym)
