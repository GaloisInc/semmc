{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Prelude hiding (reverse)

import           Control.Applicative ( Const(..) )
import           Control.Exception
import qualified Data.ByteString as BS
import qualified Data.Foldable as F

import           Control.Monad.Identity
import           Control.Monad ( liftM, forM )
import           Data.Maybe ( catMaybes )

import           Data.Kind
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Classes
import           Data.Parameterized.HasRepr
import qualified Data.Parameterized.SymbolRepr
import qualified Data.Parameterized.TraversableFC as FC
import           Data.Parameterized.Pair
import           Data.Parameterized.List as SL
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.SymbolRepr ( Symbol )
import qualified Data.Parameterized.SymbolRepr as SR
import           Data.Parameterized.Some
import           Data.Proxy
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Set as Set
import           Data.Text.Encoding ( encodeUtf8 )
import qualified Data.Text.IO as TIO
import qualified Lang.Crucible.Backend as CRUB
import qualified Lang.Crucible.Backend.Simple as S
import qualified Options.Applicative as O
import qualified Options.Applicative.Help as OH


import qualified Dismantle.ARM.T32 as T32
import qualified Dismantle.ARM.A32 as A32
import qualified Dismantle.ARM.ASL as ASL

import qualified Language.ASL.Globals as ASL

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Location as L
import qualified SemMC.BoundVar as BV
import           SemMC.Architecture
import           SemMC.Architecture.AArch32 ( AArch32 )
import qualified SemMC.Architecture.AArch32 as ARM
import qualified SemMC.Architecture.ARM.Location as ARM
import           SemMC.Architecture.ARM.Combined ( ARMOperandRepr )
import qualified SemMC.Architecture.ARM.Combined as ARM
import qualified SemMC.Architecture.ARM.ASL as ASL
import qualified SemMC.DSL as DSL
import qualified SemMC.Formula.Formula as SF
import qualified SemMC.Formula.Printer as SF
import qualified SemMC.Formula.Env as SE
import qualified SemMC.Formula.Load as FL



import qualified Data.Type.List as TL
import qualified Data.Parameterized.TyMap as TM

import qualified System.Directory as D
import           System.Exit
import           System.FilePath ( (<.>), (</>) )

import qualified What4.Serialize.Parser as WP

import qualified What4.Interface as WI
import qualified What4.Expr.Builder as WB
import qualified What4.Utils.Log as Log
import           What4.Utils.Util ( SomeSome(..) )
import qualified SemMC.Util as U

import Debug.Trace

data Options = Options { oRootDir :: FilePath
                       , oSrcFile :: FilePath
                       , oNoCheck :: Bool
                       }

optionsParser :: O.Parser Options
optionsParser = Options
                <$> O.strArgument ( O.metavar "ROOTDIR"
                                  <> O.help "Root directory under which output files will be \
                                            \ generated (\"data/sem\" is recommended).")
                <*> O.strArgument ( O.metavar "FILE"
                                  <> O.help "Input formulas file from asl-translator.")
                <*> O.switch ( O.long "nocheck"
                             <> O.short 'n'
                             <> O.help "Disable checking of semantics formula before writing")


main :: IO ()
main = O.execParser optParser >>= mainWithOptions
  where
    optParser = (O.info (optionsParser O.<**> O.helper)
                 ( O.fullDesc
                 <> O.header "semmc-asl-genbase - Print out translated formulas"
                 )) { O.infoProgDesc = OH.vsepChunks
                      [
                      ]
                    }

addFormulaEnv :: forall sym
              . WI.IsSymExprBuilder sym
             => sym
             -> SE.FormulaEnv sym AArch32
             -> [(T.Text, SomeSome (WI.SymFn sym))]
             -> IO (SE.FormulaEnv sym AArch32)
addFormulaEnv sym env funs = do
  let reshaped = Map.fromList $ map reshape funs
  return $ env { SE.envFunctions = (SE.envFunctions env) `Map.union` reshaped }
  where
    reshape :: (T.Text, SomeSome (WI.SymFn sym))
            -> (String, (SE.SomeSome (WI.SymFn sym), Some WI.BaseTypeRepr))
    reshape (nm, SomeSome symFn) = (swapUFPrefix nm, (SE.SomeSome symFn, Some (WI.fnReturnType symFn)))

swapUFPrefix :: T.Text -> String
swapUFPrefix nm = case T.stripPrefix "uf." nm of
  Just nm' -> "df." ++ T.unpack nm'
  Nothing -> "df." ++ T.unpack nm

dropUFPrefix :: String -> String
dropUFPrefix nm = case List.stripPrefix "uf." nm of
  Just nm' -> nm'
  Nothing -> nm

sanitizedName :: String -> String
sanitizedName name = map (\c -> case c of ' ' -> '_'; '.' -> '_'; _ -> c) name

mkEncoding :: (sym ~ WB.ExprBuilder t st fs)
           => sym
           -> WP.SymFnEnv sym
           -> ARM.ARMOpcode ARM.ARMOperand sh
           -> ASL.Encoding
           -> IO (Maybe (String, Pair (ARM.ARMOpcode ARM.ARMOperand) (SF.ParameterizedFormula sym AArch32)))
mkEncoding sym symfns opcode enc = do
  ASL.encodingToFormula sym symfns opcode enc >>= \case
    Just formula -> return $ Just $ (ASL.encName enc, Pair opcode formula)
    Nothing -> return Nothing

-- clagged from SemMC.Formula.Load, except we avoid prefixing function symbols with "uf"
-- in order to emit the correct names during printing
formulaEnv :: forall sym arch
            . (A.Architecture arch, WI.IsSymExprBuilder sym)
           => Proxy arch
           -> sym
           -> IO (SE.FormulaEnv sym arch)
formulaEnv proxy sym = do
  undefinedBit <- WI.freshConstant sym (U.makeSymbol "undefined_bit") knownRepr
  ufs <- Map.fromList <$> mapM toUF (A.uninterpretedFunctions proxy)
  return SE.FormulaEnv { SE.envFunctions = ufs
                       , SE.envUndefinedBit = undefinedBit
                       }
  where
    toUF :: A.UninterpFn arch
         -> IO (String, (SE.SomeSome (WI.SymFn sym), Some WI.BaseTypeRepr))
    toUF (A.MkUninterpFn name args ret _) = do
      uf <- SE.SomeSome <$> WI.freshTotalUninterpFn sym (U.makeSymbol name) args ret
      return (("uf." ++ sanitizedName name), (uf, Some ret))


readSymFns :: forall sym
            . (WI.IsSymExprBuilder sym, ShowF (WI.SymExpr sym))
           => Options
           -> sym
           -> SE.FormulaEnv sym AArch32
           -> IO (Map.Map T.Text (SomeSome (WI.SymFn sym)))
readSymFns opts sym env = do
  let pcfg = (WP.defaultParserConfig sym) {WP.pSymFnEnv = getParserEnv env}
  lcfg <- Log.mkLogCfg "main"
  Log.withLogCfg lcfg $
    WP.readSymFnEnvFromFile pcfg (oSrcFile opts) >>= \case
      Left err -> fail err
      Right symfns -> return symfns
  where
    getParserEnv :: SE.FormulaEnv sym AArch32 -> Map.Map T.Text (SomeSome (WI.SymFn sym))
    getParserEnv (SE.FormulaEnv env _) = Map.fromList $ map reshape $ Map.assocs env

    reshape :: (String, (SE.SomeSome (WI.SymFn sym), Some WI.BaseTypeRepr)) -> (T.Text, (SomeSome (WI.SymFn sym)))
    reshape (nm, (SE.SomeSome symfn, _)) = (T.pack $ nm, SomeSome symfn)


mainWithOptions :: Options -> IO ()
mainWithOptions opts = do
  Some ng <- PN.newIONonceGenerator
  sym <- S.newSimpleBackend S.FloatIEEERepr ng
  initenv <- formulaEnv (Proxy @ARM.AArch32) sym
  putStrLn $ "Parsing " ++ oSrcFile opts
  symfns <- readSymFns opts sym initenv
  a32pfs <- liftM catMaybes $ forM (Map.assocs A32.aslEncodingMap) $ \(Some a32opcode, enc) ->
    mkEncoding sym symfns (ARM.A32Opcode a32opcode) enc
  t32pfs <- liftM catMaybes $ forM (Map.assocs T32.aslEncodingMap) $ \(Some t32opcode, enc) ->
    mkEncoding sym symfns (ARM.T32Opcode t32opcode) enc
  let funs = Map.assocs symfns
  let lib' = ASL.symFnsToLibrary sym funs
  env <- addFormulaEnv sym initenv funs
  let odir = oRootDir opts
      genFunsTo d l = do
        ans@(s, e, _lib) <- genFunDefs sym env d (not $ oNoCheck opts) l
        putStrLn $ "Wrote " <> (show s) <> " function files to " <> d <>
                       (if 0 == e then "" else " (" <> show e <> " errors!)")
        return ans
      genTo d lib (s,e) l = do
        (s', e') <- genOpDefs sym env lib d (not $ oNoCheck opts) l
        putStrLn $ "Wrote " <> (show s') <> " " <> " semantics files to " <> d <>
                       (if 0 == e' then "" else " (" <> show e' <> " errors!)")
        return (s+s', e+e')
  D.createDirectoryIfMissing True odir
  let fundefs = map (\(Pair (SF.FunctionRef nm _ _) def) -> (dropUFPrefix nm, Some def)) $ MapF.toList lib'
  (s0,e0,lib) <- genFunsTo odir fundefs
  (s,e) <- F.foldlM (genTo odir lib) (s0,e0) (a32pfs ++ t32pfs)
  putStrLn $ "Finished writing " <> show s <> " files with " <> show e <> " errors."
  if e > 0 then exitFailure else exitSuccess


genFunDefs :: ( sym ~ WB.ExprBuilder t st fs
              , CRUB.IsSymInterface sym
              , ShowF (WI.SymExpr sym) )
           => sym -> SE.FormulaEnv sym (ARM.AArch32)
           -> FilePath -> Bool -> [(String, Some (SF.FunctionFormula sym))]
           -> IO (Int, Int, SF.Library sym)
genFunDefs sym env d chk l = F.foldlM writeFunDef (0, 0, SF.emptyLibrary) l
    where writeFunDef (s,e,lib) (funName, Some def) =
              let fundef  = SF.printFunctionFormula def
                  fundefB = encodeUtf8 fundef
                  writeIt = TIO.writeFile (d </> funName <.> "fun") $ fundef <> "\n"
              in if chk
                 then checkFunction (Proxy @ARM.AArch32) sym env fundefB funName >>= \case
                         Right lib' -> writeIt >> return (s+1, e, lib' `MapF.union` lib)
                         Left err -> do writeIt
                                        putStrLn $ "Error for function " <> funName <> ": " <> err
                                        return (s+1, e+1, lib)
                 else do writeIt
                         return (s+1, e, lib)


genOpDefs :: ( sym ~ WB.ExprBuilder t st fs
             , CRUB.IsSymInterface sym
             , ShowF (WI.SymExpr sym) )
          => sym -> SE.FormulaEnv sym ARM.AArch32 -> SF.Library sym
          -> FilePath -> Bool -> (String, Pair (ARM.ARMOpcode ARM.ARMOperand) (SF.ParameterizedFormula sym AArch32)) -> IO (Int, Int)
genOpDefs sym env lib d chk l = writeDef (0, 0) l
    where writeDef (s,e) (opName, Pair opcode formula) =
              let semdef  = SF.printParameterizedFormula (typeRepr opcode) formula
                  semdefB = encodeUtf8 semdef
                  writeIt = TIO.writeFile (d </> opName <.> "sem") $ semdef <> "\n"
              in if chk then
                checkFormula (Proxy @ARM.AArch32) sym env lib semdefB (Some opcode) >>= \case
                  Nothing -> writeIt >> return (s+1, e)
                  Just err -> do writeIt
                                 putStrLn $ "Error for " <> opName <> ": " <> err
                                 return (s+1, e+1)
                 else do writeIt
                         return (s+1, e)

checkFunction :: ( CRUB.IsSymInterface sym
                 , ShowF (WI.SymExpr sym)
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
                , ShowF (WI.SymExpr sym)
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
                , ShowF (WI.SymExpr sym)
                , HasRepr (ARM.ARMOpcode ARM.ARMOperand) (SL.List (OperandTypeRepr arch))
                ) =>
                Proxy arch
             -> sym
             -> SE.FormulaEnv sym arch
             -> SF.Library sym
             -> BS.ByteString
             -> Some (ARM.ARMOpcode ARM.ARMOperand)
             -> IO (Maybe String)
checkFormula arch sym env lib sem op =
  -- If the semantics are bad, loadFormula will throw an exception
  U.withLogging "semmc-ppc-genbase"
      (U.stdErrLogEventConsumer (\le -> U.leLevel le >= U.Warn)) $
      catch (loadFormula arch sym env lib (op, sem) >> return Nothing) $
          \(e :: SomeException) -> return $ Just $ show e


loadFormula :: ( CRUB.IsSymInterface sym
               , ShowF (WI.SymExpr sym)
               , Architecture arch
               , HasRepr (ARM.ARMOpcode ARM.ARMOperand) (SL.List (OperandTypeRepr arch))
               , U.HasLogCfg
               ) =>
               Proxy arch
            -> sym
            -> SE.FormulaEnv sym arch
            -> SF.Library sym
            -> (Some (ARM.ARMOpcode ARM.ARMOperand), BS.ByteString)
            -> IO (MapF.MapF (ARM.ARMOpcode ARM.ARMOperand) (SF.ParameterizedFormula sym arch))
loadFormula _ sym env lib a = FL.loadFormulas sym env lib [a]
