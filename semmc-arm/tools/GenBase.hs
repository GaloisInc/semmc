{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Control.Exception
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import           Data.Parameterized.Classes
import           Data.Parameterized.HasRepr
import           Data.Parameterized.List as SL
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.Some
import           Data.Proxy
import           Data.Text.Encoding ( encodeUtf8 )
import qualified Data.Text.IO as TIO
import qualified Lang.Crucible.Backend as CRUB
import qualified Lang.Crucible.Backend.Simple as S
import qualified Options.Applicative as O
import qualified Options.Applicative.Help as OH
import           SemMC.Architecture
import qualified SemMC.Architecture.AArch32 as ARMSem
import qualified SemMC.Architecture.ARM.BaseSemantics as B
import           SemMC.Architecture.ARM.Opcodes ( allA32Opcodes, allT32Opcodes )
import qualified SemMC.DSL as DSL
import qualified SemMC.Formula.Formula as SF
import qualified SemMC.Formula.Env as SE
import qualified SemMC.Formula.Load as FL
import qualified SemMC.Util as U
import qualified System.Directory as D
import           System.Exit
import           System.FilePath ( (<.>), (</>) )
import qualified What4.Interface as CRU


data Options = Options { oRootDir :: FilePath
                       , oNoCheck :: Bool
                       }

optionsParser :: O.Parser Options
optionsParser = Options
                <$> O.strArgument ( O.metavar "ROOTDIR"
                                  <> O.help "Root directory under which output files will be \
                                            \ generated (\"data/sem\" is recommended).")
                <*> O.switch ( O.long "nocheck"
                             <> O.short 'n'
                             <> O.help "Disable checking of semantics formula before writing")


main :: IO ()
main = O.execParser optParser >>= mainWithOptions
  where
    optParser = (O.info (optionsParser O.<**> O.helper)
                 ( O.fullDesc
                 <> O.header "semmc-arm-genbase - Print out manually-written formulas"
                 )) { O.infoProgDesc = OH.vsepChunks
                      [ OH.paragraph
                        "Generates the base and manual formula set files for ARM.\
                        \ The semmc tool will learn semantics for many instructions,\
                        \ but it needs to start with a base set, and there are some\
                        \ that it cannot learn and which must be specified manually."
                      , OH.paragraph
                        "This tool is used to generate those base and manual formulas,\
                        \ as written in the DSL specifications in the\
                        \ SemMC.Architecture.ARM.BaseSemantics module."
                      ]
                    }


mainWithOptions :: Options -> IO ()
mainWithOptions opts = do
  Some ng <- PN.newIONonceGenerator
  sym <- S.newSimpleBackend S.FloatIEEERepr ng
  env <- FL.formulaEnv (Proxy @ARMSem.AArch32) sym
  let odir = oRootDir opts
      genFunsTo d l = do
        ans@(s, e, _lib) <- genFunDefs sym env d (not $ oNoCheck opts) l
        putStrLn $ "Wrote " <> (show s) <> " function files to " <> d <>
                       (if 0 == e then "" else " (" <> show e <> " errors!)")
        return ans
      genTo d lib (s,e) (t,l) = do
        (s', e') <- genOpDefs sym env lib d (not $ oNoCheck opts) l
        putStrLn $ "Wrote " <> (show s') <> " " <> t <> " semantics files to " <> d <>
                       (if 0 == e' then "" else " (" <> show e' <> " errors!)")
        return (s+s', e+e')
  D.createDirectoryIfMissing True odir
  (s0,e0,lib) <- genFunsTo odir B.fundefs
  (s,e) <- F.foldlM (genTo odir lib) (s0,e0) B.semdefs
  putStrLn $ "Finished writing " <> show s <> " files with " <> show e <> " errors."
  if e > 0 then exitFailure else exitSuccess


genFunDefs :: ( CRUB.IsSymInterface sym
              , ShowF (CRU.SymExpr sym) )
           => sym -> SE.FormulaEnv sym (ARMSem.AArch32)
           -> FilePath -> Bool -> [(String, DSL.FunctionDefinition)]
           -> IO (Int, Int, SF.Library sym)
genFunDefs sym env d chk l = F.foldlM writeFunDef (0, 0, SF.emptyLibrary) l
    where writeFunDef (s,e,lib) (funName, def) =
              let fundef  = DSL.printFunctionDefinition def
                  fundefB = encodeUtf8 fundef
                  writeIt = TIO.writeFile (d </> funName <.> "fun") $ fundef <> "\n"
              in if chk
                 then checkFunction (Proxy @ARMSem.AArch32) sym env fundefB funName >>= \case
                         Right lib' -> writeIt >> return (s+1, e, lib' `MapF.union` lib)
                         Left err -> do writeIt
                                        putStrLn $ "Error for function " <> funName <> ": " <> err
                                        return (s+1, e+1, lib)
                 else do writeIt
                         return (s+1, e, lib)


genOpDefs :: ( CRUB.IsSymInterface sym
             , ShowF (CRU.SymExpr sym) )
          => sym -> SE.FormulaEnv sym ARMSem.AArch32 -> SF.Library sym
          -> FilePath -> Bool -> [(String, DSL.Definition)] -> IO (Int, Int)
genOpDefs sym env lib d chk l = F.foldlM writeDef (0, 0) l
    where writeDef (s,e) (opName, def) =
              let semdef  = DSL.printDefinition def
                  semdefB = encodeUtf8 semdef
                  opcode = F.find ((==) opName . show) opcodes
                  writeIt = TIO.writeFile (d </> opName <.> "sem") $ semdef <> "\n"
              in case opcode of
                   Nothing -> do putStrLn $ "ERR: ignoring unknown DSL defined opcode \"" <>
                                          opName <> "\" (-> " <> d <> ")"
                                 return (s, e+1)
                   Just op -> if chk
                              then checkFormula (Proxy @ARMSem.AArch32) sym env lib semdefB op >>= \case
                                     Nothing -> writeIt >> return (s+1, e)
                                     Just err -> do writeIt
                                                    putStrLn $ "Error for " <> opName <> ": " <> err
                                                    return (s+1, e+1)

                              else do writeIt
                                      return (s+1, e)
          opcodes = allA32Opcodes ++ allT32Opcodes

checkFunction :: ( CRUB.IsSymInterface sym
                 , ShowF (CRU.SymExpr sym)
                 , Architecture arch )
              => Proxy arch
              -> sym
              -> SE.FormulaEnv sym arch
              -> BS.ByteString
              -> String
              -> IO (Either String (SF.Library sym))
checkFunction arch sym env sem name =
  U.withLogging "semmc-ppc-genbase"
      (U.stdErrLogEventConsumer (\le -> U.leLevel le >= U.Warn)) $
      catch (Right <$> loadFunction arch sym env (name, sem)) $
                 \(e :: SomeException) -> return $ Left $ show e

loadFunction :: ( CRUB.IsSymInterface sym
                , ShowF (CRU.SymExpr sym)
                , Architecture arch
                , U.HasLogCfg )
             => Proxy arch
             -> sym
             -> SE.FormulaEnv sym arch
             -> (String, BS.ByteString)
             -> IO (SF.Library sym)
loadFunction arch sym env pair = FL.loadLibrary arch sym env [pair]

checkFormula :: ( Architecture arch
                , CRUB.IsSymInterface sym
                , ShowF (CRU.SymExpr sym)
                , HasRepr (ARMSem.ARMOpcode ARMSem.ARMOperand) (SL.List (OperandTypeRepr arch))
                ) =>
                Proxy arch
             -> sym
             -> SE.FormulaEnv sym arch
             -> SF.Library sym
             -> BS.ByteString
             -> Some (ARMSem.ARMOpcode ARMSem.ARMOperand)
             -> IO (Maybe String)
checkFormula arch sym env lib sem op =
  -- If the semantics are bad, loadFormula will throw an exception
  U.withLogging "semmc-ppc-genbase"
      (U.stdErrLogEventConsumer (\le -> U.leLevel le >= U.Warn)) $
      catch (loadFormula arch sym env lib (op, sem) >> return Nothing) $
          \(e :: SomeException) -> return $ Just $ show e


loadFormula :: ( CRUB.IsSymInterface sym
               , ShowF (CRU.SymExpr sym)
               , Architecture arch
               , HasRepr (ARMSem.ARMOpcode ARMSem.ARMOperand) (SL.List (OperandTypeRepr arch))
               , U.HasLogCfg
               ) =>
               Proxy arch
            -> sym
            -> SE.FormulaEnv sym arch
            -> SF.Library sym
            -> (Some (ARMSem.ARMOpcode ARMSem.ARMOperand), BS.ByteString)
            -> IO (MapF.MapF (ARMSem.ARMOpcode ARMSem.ARMOperand) (SF.ParameterizedFormula sym arch))
loadFormula _ sym env lib a = FL.loadFormulas sym env lib [a]
