{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main ( main ) where

import           Control.Exception
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import           Data.Monoid
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.Some
import           Data.Parameterized.HasRepr
import           Data.Parameterized.List as SL
import           Data.Proxy
import           Data.Text.Encoding ( encodeUtf8 )
import qualified Data.Text.IO as TIO
import qualified Dismantle.PPC as PPC
import qualified Lang.Crucible.Solver.Interface as CRU
import qualified Lang.Crucible.Solver.SimpleBackend as S
import qualified Options.Applicative as O
import           SemMC.Architecture
import qualified SemMC.Architecture.PPC.Base as B
import qualified SemMC.Architecture.PPC32 as PPC32Sem
import qualified SemMC.Architecture.PPC32.Opcodes as PPC32
import qualified SemMC.Architecture.PPC64 as PPC64Sem
import qualified SemMC.Architecture.PPC64.Opcodes as PPC64
import qualified SemMC.DSL as DSL
import qualified SemMC.Formula.Formula as SF
import qualified SemMC.Formula.Load as FL
import qualified SemMC.Util as U
import qualified System.Directory as D
import           System.FilePath ( (<.>), (</>) )

data Options = Options { oManualDir :: FilePath
                       , oBaseDir :: FilePath
                       , oPseudoDir :: FilePath
                       , oBitSize :: B.BitSize
                       , oNoCheck :: Bool
                       }

optionsParser :: O.Parser Options
optionsParser = Options <$> O.strOption ( O.long "manual"
                                        <> O.short 'M'
                                        <> O.metavar "DIR"
                                        <> O.help "The directory to store manual formulas in" )
                        <*> O.strOption ( O.long "base"
                                        <> O.short 'B'
                                        <> O.metavar "DIR"
                                        <> O.help "The directory to store base formulas in" )
                        <*> O.strOption ( O.long "pseudo"
                                        <> O.short 'P'
                                        <> O.metavar "DIR"
                                        <> O.help "The directory to store pseudo-operation formulas in" )
                        <*> O.option O.auto ( O.long "bit-size"
                                            <> O.short 's'
                                            <> O.metavar "SIZE"
                                            <> O.help "The bit size to use (Size32 or Size64)" )
                        <*> O.switch ( O.long "nocheck"
                                     <> O.short 'n'
                                     <> O.help "Disable checking of semantics formula before writing")

main :: IO ()
main = O.execParser optParser >>= mainWithOptions
  where
    optParser = O.info (optionsParser O.<**> O.helper)
                ( O.fullDesc
                <> O.progDesc "Print the base and manual formula sets for PPC"
                <> O.header "semmc-ppc-genbase - Print out manually-written formulas"
                )

mainWithOptions :: Options -> IO ()
mainWithOptions opts = do
  D.createDirectoryIfMissing True (oManualDir opts)
  D.createDirectoryIfMissing True (oBaseDir opts)
  D.createDirectoryIfMissing True (oPseudoDir opts)
  let sz = oBitSize opts
  genTo (oManualDir opts) sz (B.manual sz)
  genTo (oBaseDir   opts) sz (B.base   sz)
  genTo (oPseudoDir opts) sz (B.pseudo sz)
      where genTo d sz l = do (s, e) <- genOpDefs d sz (not $ oNoCheck opts) l
                              putStrLn $ "Wrote " <> show s <> " files to " <> d <>
                                           (if 0 == e then "" else " (" <> show e <> " errors!)")

genOpDefs :: FilePath -> B.BitSize -> Bool -> [(String, DSL.Definition)] -> IO (Int, Int)
genOpDefs d sz chk l = F.foldlM writeDef (0, 0) l
    where writeDef (s,e) (opName, def) =
              let opcode = F.find ((==) opName . show) opcodes
                  semdef = DSL.printDefinition def
                  semdefB = encodeUtf8 semdef
                  writeIt = TIO.writeFile (d </> opName <.> "sem") $ semdef <> "\n"
              in case opcode of
                   Nothing -> do putStrLn ("Warning: unknown DSL defined opcode \"" <>
                                           opName <> "\" (-> " <> d <> ")")
                                 writeIt
                                 return (s+1, e+1)
                   Just op -> if chk
                              -- then checkFormula arch semdefB op >>= \case
                              then do r <- case sz of
                                             B.Size32 -> checkFormula (Proxy @PPC32Sem.PPC) semdefB op
                                             B.Size64 -> checkFormula (Proxy @PPC64Sem.PPC) semdefB op
                                      case r of
                                        Nothing -> writeIt >> return (s+1, e)
                                        Just err -> do putStrLn ("Error for " <> opName <> ": " <> err)
                                                       return (s, e+1)
                              else writeIt >> return (s+1, e)
          opcodes = case sz of
                      B.Size32 -> PPC32.allOpcodes
                      B.Size64 -> PPC64.allOpcodes

checkFormula :: ( Architecture arch
                , HasRepr (PPC.Opcode PPC.Operand) (SL.List (OperandTypeRepr arch))
                ) =>
                Proxy arch
             -> BS.ByteString
             -> Some (PPC.Opcode PPC.Operand)
             -> IO (Maybe String)
checkFormula arch sem op =
    do Some ng <- PN.newIONonceGenerator
       sym <- S.newSimpleBackend ng
       -- If the semantics are bad, loadFormula will throw an exception
       U.withLogging "semmc-ppc-genbase"
            (U.stdErrLogEventConsumer (\le -> U.leLevel le >= U.Warn)) $
            catch (loadFormula arch sym (op, sem) >> return Nothing) $
                      \(e :: SomeException) -> return $ Just $ show e


loadFormula :: ( CRU.IsSymInterface sym
               , ShowF (CRU.SymExpr sym)
               , Architecture arch
               , HasRepr (PPC.Opcode PPC.Operand) (SL.List (OperandTypeRepr arch))
               , U.HasLogCfg
               ) =>
               Proxy arch
            -> sym
            -> (Some (PPC.Opcode PPC.Operand), BS.ByteString)
            -> IO (MapF.MapF (PPC.Opcode PPC.Operand) (SF.ParameterizedFormula sym arch))
loadFormula _ sym a = FL.loadFormulas sym [a]
