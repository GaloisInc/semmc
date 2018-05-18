{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Control.Exception
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import           Data.Monoid
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
import qualified SemMC.ARM as ARMSem
import           SemMC.Architecture
import qualified SemMC.Architecture.ARM.BaseSemantics as B
import           SemMC.Architecture.ARM.Opcodes ( allA32Opcodes, allT32Opcodes )
import qualified SemMC.DSL as DSL
import qualified SemMC.Formula.Formula as SF
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
  let odir = oRootDir opts
      genTo d (s,e) (t,l) = do
        (s', e') <- genOpDefs d (not $ oNoCheck opts) l
        putStrLn $ "Wrote " <> (show s') <> " " <> t <> " semantics files to " <> d <>
                       (if 0 == e' then "" else " (" <> show e' <> " errors!)")
        return (s+s', e+e')
  D.createDirectoryIfMissing True odir
  (s,e) <- F.foldlM (genTo odir) (0,0) B.semdefs
  putStrLn $ "Finished writing " <> show s <> " semantics files with " <> show e <> " errors."
  if e > 0 then exitFailure else exitSuccess


genOpDefs :: FilePath -> Bool -> [(String, DSL.Definition)] -> IO (Int, Int)
genOpDefs d chk l = F.foldlM writeDef (0, 0) l
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
                              then checkFormula (Proxy @ARMSem.ARM) semdefB op >>= \case
                                     Nothing -> writeIt >> return (s+1, e)
                                     Just err -> do putStrLn $ "Error for " <> opName <> ": " <> err
                                                    return (s, e+1)

                              else do writeIt
                                      return (s+1, e)
          opcodes = allA32Opcodes ++ allT32Opcodes


checkFormula :: ( Architecture arch
                , HasRepr (ARMSem.ARMOpcode ARMSem.ARMOperand) (SL.List (OperandTypeRepr arch))
                ) =>
                Proxy arch
             -> BS.ByteString
             -> Some (ARMSem.ARMOpcode ARMSem.ARMOperand)
             -> IO (Maybe String)
checkFormula arch sem op =
    do Some ng <- PN.newIONonceGenerator
       sym <- S.newSimpleBackend ng
       -- If the semantics are bad, loadFormula will throw an exception
       U.withLogging "semmc-ppc-genbase"
            (U.stdErrLogEventConsumer (\le -> U.leLevel le >= U.Warn)) $
            catch (loadFormula arch sym (op, sem) >> return Nothing) $
                      \(e :: SomeException) -> return $ Just $ show e


loadFormula :: ( CRUB.IsSymInterface sym
               , ShowF (CRU.SymExpr sym)
               , Architecture arch
               , HasRepr (ARMSem.ARMOpcode ARMSem.ARMOperand) (SL.List (OperandTypeRepr arch))
               , U.HasLogCfg
               ) =>
               Proxy arch
            -> sym
            -> (Some (ARMSem.ARMOpcode ARMSem.ARMOperand), BS.ByteString)
            -> IO (MapF.MapF (ARMSem.ARMOpcode ARMSem.ARMOperand) (SF.ParameterizedFormula sym arch))
loadFormula _ sym a = FL.loadFormulas sym [a]
