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
module SemMC.TH
  ( attachSemantics
  , loadSemantics
  )
where

import qualified Control.Exception as E
import           Control.Monad ( forM_ )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as UBS
import           Data.Maybe ( catMaybes )
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax ( qAddDependentFile )
import           System.FilePath ( (</>) )
import qualified System.IO.Unsafe as IO

import           Data.Parameterized.Lift ( LiftF(..) )
import           Data.Parameterized.Some ( Some(..) )


-- | Given a list of opcodes and a function for turning each opcode into a file
-- name, search for the file with that name in each of the directories.  If it
-- is found, pair the original opcode with a bytestring containing the file's
-- contents.
attachSemantics :: (LiftF a)
                => (Some a -> FilePath)
                -- ^ A function to convert opcodes to filenames
                -> [Some a]
                -- ^ A list of opcodes
                -> [FilePath]
                -- ^ A list of directories to search for the produced filenames
                -> Q Exp
attachSemantics toFP elts dirs = do
  ops <- runIO $ loadSemantics toFP elts dirs
  forM_ ops $ \(f, _, _) -> qAddDependentFile f
  listE (map toOpcodePair ops)

-- | The IO-only version of attachSemantics.
loadSemantics :: (Some a -> FilePath)
              -- ^ A function to convert opcodes to filenames
              -> [Some a]
              -- ^ A list of opcodes
              -> [FilePath]
              -- ^ A list of directories to search for the produced filenames
              -> IO [(FilePath, Some a, BS.ByteString)]
loadSemantics toFP elts dirs =
    catMaybes <$> mapM (findCorrespondingFile toFP dirs) elts

toOpcodePair :: (LiftF a) => (FilePath, Some a, BS.ByteString) -> ExpQ
toOpcodePair (_, Some o, bs) = tupE [ [| Some $(liftF o) |], bsE]
  where
    len = BS.length bs
    bsE = [| IO.unsafePerformIO (UBS.unsafePackAddressLen len $(litE (StringPrimL (BS.unpack bs)))) |]

findCorrespondingFile :: (Some a  -> FilePath)
                      -> [FilePath]
                      -> Some a
                      -> IO (Maybe (FilePath, Some a, BS.ByteString))
findCorrespondingFile toFP dirs elt = go files
  where
    files = [ dir </> toFP elt | dir <- dirs ]
    go [] = return Nothing
    go (f:rest) = do
      mbs <- E.try (BS.readFile f)
      case mbs of
        Left (_::E.SomeException) -> go rest
        Right bs -> do
          return (Just (f, elt, bs))
