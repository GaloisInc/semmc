{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
-- | This module contains the definitions of memory synchronization instructions.
--
-- Note that many of the semantics here are *vastly* simplified and do not
-- encode any of the synchronization semantics.  We don't currently have a way
-- to talk about those yet.  Once we have one, these semantic will need to be
-- rewritten.
module SemMC.Architecture.PPC.Base.Sync (
  baseSync
  ) where

import Prelude hiding ( concat )
import Control.Monad ( when )
import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core
import SemMC.Architecture.PPC.Base.Core.Memory

baseSync :: (?bitSize :: BitSize) => SemM 'Top ()
baseSync = do
  defineOpcodeWithIP "LBARX" $ do
    comment "Load Byte and Reserve Indexed (X-form)"
    comment "This is basically LBZX with additional memory synchronization semantics"
    loadIndexed 1 id zext
  defineOpcodeWithIP "LHARX" $ do
    comment "Load Halfword and Reserve Indexed (X-form)"
    comment "This is basically LHZX with additional memory synchronization semantics"
    loadIndexed 2 id zext
  defineOpcodeWithIP "LWARX" $ do
    comment "Load Word and Reserve Indexed (X-form)"
    comment "This is basically LWZX with additional memory synchronization semantics"
    loadIndexed 4 id zext

  defineOpcodeWithIP "LBARXL" $ do
    comment "Load Byte and Reserve Indexed (X-form)"
    comment "This is basically LBZX with additional memory synchronization semantics"
    comment "This is a variant of LBARX with a hint about future updates to the storage"
    loadIndexed 1 id zext
  defineOpcodeWithIP "LHARXL" $ do
    comment "Load Halfword and Reserve Indexed (X-form, EH=1)"
    comment "This is basically LHZX with additional memory synchronization semantics"
    comment "This is a variant of LHARX with a hint about future updates to the storage"
    loadIndexed 2 id zext
  defineOpcodeWithIP "LWARXL" $ do
    comment "Load Word and Reserve Indexed (X-form, EH=1)"
    comment "This is basically LWZX with additional memory synchronization semantics"
    comment "This is a variant of LWARX with a hint about future updates to the storage"
    loadIndexed 4 id zext

  defineOpcodeWithIP "STBCX" $ do
    comment "Store Byte Conditional Indexed (X-form)"
    comment "The manual lists this with a . since it always updates CR0"
    storeIndexed 1 id
    updateCR0

  defineOpcodeWithIP "STHCX" $ do
    comment "Store Halfword Conditional Indexed (X-form)"
    comment "The manual lists this with a . since it always updates CR0"
    storeIndexed 2 id
    updateCR0

  defineOpcodeWithIP "STWCX" $ do
    comment "Store Word Conditional Indexed (X-form)"
    comment "The manual lists this with a . since it always updates CR0"
    storeIndexed 4 id
    updateCR0

  when (?bitSize == Size64) $ do
    defineOpcodeWithIP "LDARX" $ do
      comment "Load Doubleword and Reserve Indexed (X-form)"
      comment "This is basically LDX with additional memory synchronization semantics"
      loadIndexed 8 id id

    defineOpcodeWithIP "LDARXL" $ do
      comment "Load Doubleword and Reserve Indexed (X-form, EH=1)"
      comment "This is basically LDX with additional memory synchronization semantics"
      comment "This is a variant of LDARX with a hint about future updates to the storage"
      loadIndexed 8 id id

    defineOpcodeWithIP "STDCX" $ do
      comment "Store Doubleword Conditional Indexed (X-form)"
      comment "The manual lists this with a . since it always updates CR0"
      storeIndexed 8 id
      updateCR0

-- | This helper does the update of CR0 required by the synchronized store
-- instructions.
--
-- It always assumes that the store was performed (i.e., that the reservation
-- didn't fail).  This isn't really right in a multi-threaded environment.
updateCR0 :: (?bitSize :: BitSize) => SemM 'Def ()
updateCR0 = do
  input cr
  input xer
  let so = xerBit SO (Loc xer)
  let crnibble = concat (concat (LitBV 2 0x0) (LitBV 1 0x1)) so
  defLoc cr (bvor (Loc cr) (zext' 32 crnibble))
