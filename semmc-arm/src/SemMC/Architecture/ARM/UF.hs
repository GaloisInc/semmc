{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module SemMC.Architecture.ARM.UF
    ( uninterpretedFunctions
    )
    where

import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Some ( Some(..) )
import           GHC.TypeLits
import           Lang.Crucible.BaseTypes
import           SemMC.Architecture.ARM.Location


uninterpretedFunctions :: -- forall proxy arm
                        -- . (KnownNat (ArchRegWidth arm), 1 <= ArchRegWidth arm)
                       -- =>
                       proxy arm
                       -> [(String, Some (Ctx.Assignment BaseTypeRepr), Some BaseTypeRepr)]
uninterpretedFunctions _ =
  [ ("fp.add64",
     Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseBVType 64 Ctx.::> BaseBVType 64)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 64)))
  ]
