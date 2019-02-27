{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module SemMC.Architecture.ARM.UFgen
    ( ufGen
    , ufRGen
    , ufWGen
    )
    where

import Data.Parameterized.Context
import Data.Parameterized.Some ( Some(..) )
import Language.Haskell.TH
import SemMC.Architecture.ARM.BaseSemantics.Registers
import What4.BaseTypes


ufGen :: String -> [Int] -> Q Exp
ufGen fn sizes = listE $ map each sizes
    where each n =
              [| ( $(litE $ stringL $ fn <> "." <> show n)
                 , Some (knownRepr :: Assignment BaseTypeRepr
                                      (EmptyCtx
                                      ::> BaseBVType $(litT $ numTyLit $ toInteger n)))
                 , Some (knownRepr :: BaseTypeRepr (BaseBVType $(litT $ numTyLit $ toInteger n)))
                 )
              |]

ufRGen :: String -> [Int] -> Q Exp
ufRGen fn sizes = listE $ map each sizes
    where each n =
              [| ( $(litE $ stringL $ fn <> "." <> show n)
                 , Some (knownRepr :: Assignment BaseTypeRepr
                                      (EmptyCtx
                                      ::> (BaseArrayType
                                           (SingleCtx (BaseBVType $(litT $ numTyLit regWidth)))
                                           (BaseBVType 8))
                                      ::> (BaseBVType $(litT $ numTyLit regWidth))))
                 , Some (knownRepr :: BaseTypeRepr (BaseBVType $(litT $ numTyLit $ toInteger n)))
                 )
              |]

ufWGen :: String -> [Int] -> Q Exp
ufWGen fn sizes = listE $ map each sizes
    where each n =
              [| ( $(litE $ stringL $ fn <> "." <> show n)
                 , Some (knownRepr :: Assignment BaseTypeRepr
                                      (EmptyCtx
                                      ::> BaseArrayType
                                        (SingleCtx (BaseBVType $(litT $ numTyLit regWidth)))
                                        (BaseBVType 8)
                                      ::> BaseBVType $(litT $ numTyLit regWidth)
                                      ::> BaseBVType $(litT $ numTyLit $ toInteger n)))
                 , Some (knownRepr :: BaseTypeRepr (BaseArrayType
                                                    (SingleCtx (BaseBVType $(litT $ numTyLit regWidth)))
                                                    (BaseBVType 8)))
                 )
              |]
