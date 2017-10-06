{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
-- | Template Haskell helpers for defining opcode lists and capturing semantics
-- files as bytestrings
--
-- We want to capture semantics files as bytestrings so that we can easily
-- access them from other packages without having to rely on their on-disk
-- locations being stable.
module SemMC.TH (
  attachSemantics
  ) where

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as UBS
import           Data.Maybe ( catMaybes )
import           System.FilePath ( (</>) )
import           Language.Haskell.TH

import           Data.Parameterized.Lift ( LiftF(..) )
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.Witness ( Witness(..) )


-- | Given a list of opcodes and a function for turning each opcode into a file
-- name, search for the file with that name in each of the directories.  If it
-- is found, pair the original opcode with a bytestring containing the file's
-- contents.
attachSemantics :: (LiftF a)
                => (Some (Witness c a) -> FilePath)
                -- ^ A function to convert opcodes to filenames
                -> [Some (Witness c a)]
                -- ^ A list of opcodes
                -> [FilePath]
                -- ^ A list of directories to search for the produced filenames
                -> Q Exp
attachSemantics toFP elts dirs = do
  ops <- catMaybes <$> runIO (mapM (findCorrespondingFile toFP dirs) elts)
  listE (map toOpcodePair ops)

toOpcodePair :: (LiftF a) => (Some (Witness c a), BS.ByteString) -> ExpQ
toOpcodePair (Some (Witness o), bs) = tupE [ [| Some (Witness $(liftF o)) |], bsE]
  where
    len = BS.length bs
    bsE = [| UBS.unsafePackAddressLen len $(litE (StringPrimL (BS.unpack bs))) |]

findCorrespondingFile :: (Some (Witness c a)  -> FilePath)
                      -> [FilePath]
                      -> Some (Witness c a)
                      -> IO (Maybe (Some (Witness c a), BS.ByteString))
findCorrespondingFile toFP dirs elt = go files
  where
    files = [ toFP elt </> dir | dir <- dirs ]
    go [] = return Nothing
    go (f:rest) =
      ((Just . (elt,)) <$> BS.readFile f) `E.catch` (\(_ex :: E.IOException) -> go rest)

