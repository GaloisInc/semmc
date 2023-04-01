{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main ( main ) where

import           Control.Exception
import           Control.Monad ( when )
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import           Data.Parameterized.HasRepr
import           Data.Parameterized.List as SL
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.Some
import           Data.Proxy
import           Data.Text.Encoding ( encodeUtf8 )
import qualified Data.Text.IO as TIO
import qualified Dismantle.PPC as PPC
import qualified Lang.Crucible.Backend as CRUB
import qualified Lang.Crucible.Backend.Simple as S
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
import qualified What4.Expr.Builder as WEB
import qualified What4.InterpretedFloatingPoint as WIF

import           Prelude

data Options = Options { oManualDir :: FilePath
                       , oBaseDir :: FilePath
                       , oPseudoDir :: FilePath
                       , oBitSize :: B.BitSize
                       , oNoCheck :: Bool
                       , oWriteOnFail :: Bool
                       , oLogProgress :: Bool
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
                        <*> O.switch ( O.long "write-on-fail"
                                     <> O.short 'f'
                                     <> O.help "Write out the formula to disk even if the semantic check fails")
                        <*> O.switch ( O.long "log-progress"
                                     <> O.short 'l'
                                     <> O.help "Write out the name of each formula/op being processed")

main :: IO ()
main = O.execParser optParser >>= mainWithOptions
  where
    optParser = O.info (optionsParser O.<**> O.helper)
                ( O.fullDesc
                <> O.progDesc "Print the base and manual formula sets for PPC"
                <> O.header "semmc-ppc-genbase - Print out manually-written formulas"
                )

data SemMCPPCData t = SemMCPPCData

mainWithOptions :: Options -> IO ()
mainWithOptions opts = do
  Some ng <- PN.newIONonceGenerator
  sym <- WEB.newExprBuilder WEB.FloatIEEERepr SemMCPPCData ng
  D.createDirectoryIfMissing True (oManualDir opts)
  D.createDirectoryIfMissing True (oBaseDir opts)
  D.createDirectoryIfMissing True (oPseudoDir opts)
  let sz = oBitSize opts
  genTo opts sym (oManualDir opts) sz (B.manual sz)
  genTo opts sym (oBaseDir   opts) sz (B.base   sz)
  genTo opts sym (oPseudoDir opts) sz (B.pseudo sz)

genTo :: ( sym ~ WEB.ExprBuilder t st fs
         , CRUB.IsSymInterface sym
         , WIF.IsInterpretedFloatExprBuilder sym
         )
      => Options
      -> sym
      -> FilePath
      -> B.BitSize
      -> DSL.Package
      -> IO ()
genTo opts sym d sz l =
  do (noErrors, errors) <-
       genOpDefs sym d sz (not $ oNoCheck opts) (oWriteOnFail opts) (oLogProgress opts) l
     putStrLn $ "Wrote " <>
       (if (oWriteOnFail opts) then (show $ noErrors + errors) else (show noErrors)) <>
       " files to " <> d <>
       (if 0 == errors then "" else " (" <> show errors <> " errors!)")

genOpDefs :: forall sym t st fs
           . ( sym ~ WEB.ExprBuilder t st fs
             , CRUB.IsSymInterface sym
             )
          => sym
          -- ^ Symbolic backend
          -> FilePath
          -- ^ File to generate
          -> B.BitSize
          -- ^ What size? (E.g., Size32 vs Size64)
          -> Bool
          -- ^ Should we check the semantics after generating the serialized format?
          -> Bool
          -- ^ Should we write out to disk even if the semantics check encounters an error?
          -> Bool
          -- ^ Should we log the name of each fun/op as we process it?
          -> DSL.Package
          -- ^ DSL package
          -> IO (Int, Int)
genOpDefs sym d sz checkSemantics writeOnFail logProgress pkg = do
  let (DSL.Package { DSL.pkgFormulas = dl, DSL.pkgFunctions = fl }) = pkg
  (s, e, lib) :: (Int, Int, SF.Library sym)
    <- F.foldlM writeFunDef (0, 0, SF.emptyLibrary) fl
  F.foldlM (writeDef lib) (s, e) dl
    where writeFunDef (s,e,lib) (funName, def) = do
            let fundef = DSL.printFunctionDefinition def
                fundefB = encodeUtf8 fundef
                writeIt = TIO.writeFile (d </> funName <.> "fun") $ fundef <> "\n"
            when logProgress $ putStrLn ("Processing function " <> funName <> "...")
            if checkSemantics
              then do r <- case sz of
                             B.Size32 -> checkFunction (Proxy @PPC32Sem.PPC) sym fundefB funName
                             B.Size64 -> checkFunction (Proxy @PPC64Sem.PPC) sym fundefB funName
                      case r of
                        Right lib' -> writeIt >> return (s+1,e,lib' `MapF.union` lib)
                        Left err -> do when writeOnFail writeIt
                                       putStrLn ("Error for function " <> funName <> ": " <> err)
                                       return (s, e+1, lib)
              else writeIt >> return (s+1, e, lib)
          writeDef lib (s,e) (opName, def) = do
              let opcode = F.find ((==) opName . show) opcodes
                  semdef = DSL.printDefinition def
                  semdefB = encodeUtf8 semdef
                  writeIt = TIO.writeFile (d </> opName <.> "sem") $ semdef <> "\n"
              when logProgress $ putStrLn ("Processing op " <> opName <> "...")
              case opcode of
                Nothing -> do putStrLn ("Warning: unknown DSL defined opcode \"" <>
                                        opName <> "\" (-> " <> d <> ")")
                              writeIt
                              return (s+1, e+1)
                Just op -> if checkSemantics
                           -- then checkFormula arch semdefB op >>= \case
                           then do r <- case sz of
                                          B.Size32 -> checkFormula (Proxy @PPC32Sem.PPC) sym lib semdefB op
                                          B.Size64 -> checkFormula (Proxy @PPC64Sem.PPC) sym lib semdefB op
                                   case r of
                                     Nothing -> writeIt >> return (s+1, e)
                                     Just err -> do when writeOnFail writeIt
                                                    putStrLn ("Error for " <> opName <> ": " <> err)
                                                    return (s, e+1)
                           else writeIt >> return (s+1, e)
          opcodes = case sz of
                      B.Size32 -> PPC32.allOpcodes
                      B.Size64 -> PPC64.allOpcodes

checkFunction :: ( sym ~ WEB.ExprBuilder t st fs
                 , CRUB.IsSymInterface sym
                 , Architecture arch
                 )
              => Proxy arch
              -> sym
              -> BS.ByteString
              -> String
              -> IO (Either String (SF.Library sym))
checkFunction arch sym sem name =
  U.withLogging "semmc-ppc-genbase"
      (U.stdErrLogEventConsumer (\le -> U.leLevel le >= U.Warn)) $
      catch (Right <$> loadFunction arch sym (name, sem)) $
                 \(e :: SomeException) -> return $ Left $ show e

loadFunction :: ( sym ~ WEB.ExprBuilder t st fs
                , CRUB.IsSymInterface sym
                , Architecture arch
                , U.HasLogCfg )
             => Proxy arch
             -> sym
             -> (String, BS.ByteString)
             -> IO (SF.Library sym)
loadFunction arch sym pair = do
  env <- FL.formulaEnv arch sym
  FL.loadLibrary arch sym env [pair]

checkFormula :: ( sym ~ WEB.ExprBuilder t st fs
                , Architecture arch
                , HasRepr (PPC.Opcode PPC.Operand) (SL.List (OperandTypeRepr arch))
                , CRUB.IsSymInterface sym
                ) =>
                Proxy arch
             -> sym
             -> SF.Library sym
             -> BS.ByteString
             -> Some (PPC.Opcode PPC.Operand)
             -> IO (Maybe String)
checkFormula arch sym lib sem op =
    do -- If the semantics are bad, loadFormula will throw an exception
       U.withLogging "semmc-ppc-genbase"
            (U.stdErrLogEventConsumer (\le -> U.leLevel le >= U.Warn)) $
            catch (loadFormula arch sym lib (op, sem) >> return Nothing) $
                      \(e :: SomeException) -> return $ Just $ show e


loadFormula :: ( sym ~ WEB.ExprBuilder t st fs
               , CRUB.IsSymInterface sym
               , Architecture arch
               , HasRepr (PPC.Opcode PPC.Operand) (SL.List (OperandTypeRepr arch))
               , U.HasLogCfg
               ) =>
               Proxy arch
            -> sym
            -> SF.Library sym
            -> (Some (PPC.Opcode PPC.Operand), BS.ByteString)
            -> IO (MapF.MapF (PPC.Opcode PPC.Operand) (SF.ParameterizedFormula sym arch))
loadFormula arch sym lib a = do
  env <- FL.formulaEnv arch sym
  FL.loadFormulas sym env lib [a]
