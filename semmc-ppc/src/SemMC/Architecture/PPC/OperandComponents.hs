{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module SemMC.Architecture.PPC.OperandComponents (
  OperandComponents(..)
  ) where

import           Data.Parameterized.Classes ( ShowF )
import           GHC.TypeLits ( Symbol )

import qualified SemMC.Architecture as A
import           SemMC.Architecture.PPC.Location ( Location )
import           What4.BaseTypes ( BaseBVType )
import qualified What4.Interface as S

data OperandComponents ppc sym (s :: Symbol) where
  OCMemri :: Location ppc tp
          -> S.SymExpr sym (BaseBVType (A.RegWidth ppc))
          -> S.SymExpr sym (BaseBVType 16)
          -> OperandComponents ppc sym s
  OCMemrix :: Location ppc tp
           -> S.SymExpr sym (BaseBVType (A.RegWidth ppc))
           -> S.SymExpr sym (BaseBVType 14)
           -> OperandComponents ppc sym s
  OCMemrr :: Location ppc tp
           -> S.SymExpr sym (BaseBVType (A.RegWidth ppc))
           -> Location ppc tp
           -> S.SymExpr sym (BaseBVType (A.RegWidth ppc))
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
