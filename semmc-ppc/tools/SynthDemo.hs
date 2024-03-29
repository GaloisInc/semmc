{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main ( main ) where

import qualified Control.Concurrent.Async as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BSHex
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS8
import qualified Data.Foldable as F
import qualified Data.Functor.Identity as I
import           Data.Monoid
import           Data.Word ( Word32 )
import qualified Options.Applicative as O
import           Text.Printf ( printf )
import           Data.Proxy (Proxy(..))

import qualified Data.ElfEdit as E
import           Data.Parameterized.Classes ( OrdF, ShowF(..) )
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as N
import           Data.Parameterized.Some ( Some (..) )
import qualified Lang.Crucible.Backend as CRUB
import qualified Lang.Crucible.Backend.Online as CBO
import qualified Lang.Crucible.LLVM.MemModel as LLVM
import qualified What4.Expr.Builder as WEB
import qualified What4.ProblemFeatures as WPF
import qualified What4.Protocol.Online as WPO

import qualified Dismantle.PPC as DPPC

import           SemMC.Architecture ( Architecture
                                    , Instruction
                                    , Location
                                    , Opcode
                                    , Operand
                                    )
import qualified SemMC.Architecture.PPC32.Opcodes as PPC32
import qualified SemMC.Formula as F
import           SemMC.Synthesis.Template ( BaseSet, TemplatedArch, unTemplate )
import qualified SemMC.Synthesis as SemMC
import qualified SemMC.Util as U

import qualified SemMC.Architecture.PPC32 as PPC32

import           Prelude

data Options = Options { oInputFile :: FilePath
                       , oOutputFile :: FilePath
                       , oOriginalWithReturn :: Maybe FilePath
                       , oAppendReturn :: Bool
                       }

options :: O.Parser Options
options = Options <$> O.strArgument ( O.metavar "FILE"
                                    <> O.help "An object file to use as a specification" )
                  <*> O.strOption ( O.long "output"
                                  <> O.short 'o'
                                  <> O.metavar "FILE"
                                  <> O.help "The file to write out" )
                  <*> O.optional ( O.strOption ( O.long "with-return"
                                               <> O.metavar "FILE"
                                               <> O.help "The file to save the original program with a return instruction appended" ))
                  <*> O.switch ( O.long "append-return"
                               <> O.short 'r'
                               <> O.help "Append a return instruction to the synthesized program" )

disassembleProgram :: BS.ByteString -> Either String [DPPC.Instruction]
disassembleProgram bs
  | BS.null bs = Right []
  | otherwise =
      case DPPC.disassembleInstruction (BSL.fromStrict bs) of
        (lengthUsed, Just insn) ->
          (insn :) <$> disassembleProgram (BS.drop lengthUsed bs)
        (lengthUsed, Nothing) ->
          let badInsnHex = BS8.toString (BSHex.encode (BS.take lengthUsed bs))
          in Left (printf "Invalid instruction \"%s\"" badInsnHex)

fromRightM :: (Monad m) => Either String a -> m a
fromRightM (Left err) = error err
fromRightM (Right val) = return val

makePlain :: forall arch sym
           . (OrdF (Opcode arch (Operand arch)),
              OrdF (Location arch))
          => BaseSet sym arch
          -> MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula sym arch)
makePlain = MapF.foldrWithKey f MapF.empty
  where f :: forall sh
           . Opcode arch (Operand arch) sh
          -> F.ParameterizedFormula sym (TemplatedArch arch) sh
          -> MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula sym arch)
          -> MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula sym arch)
        f op pf = MapF.insert op (unTemplate pf)

instantiateFormula' :: (Architecture arch, CRUB.IsSymBackend (WEB.ExprBuilder t st fs) bak)
                    => bak
                    -> MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula (WEB.ExprBuilder t st fs) arch)
                    -> Instruction arch
                    -> IO (F.Formula (WEB.ExprBuilder t st fs) arch)
instantiateFormula' bak m (DPPC.Instruction op params) = do
  case MapF.lookup op m of
    Just pf -> snd <$> F.instantiateFormula bak pf params
    Nothing -> fail (printf "Couldn't find semantics for opcode \"%s\"" (showF op))

loadProgramBytes :: FilePath -> IO (E.Elf 32, E.ElfSection Word32)
loadProgramBytes fp = do
  objFile <- BS.readFile fp
  elf <- case E.parseElf objFile of
           E.Elf32Res _ e -> return e
           _err -> fail "Expected an Elf32, but got, well, something else"
  textSection <- case E.findSectionByName (BS8.fromString ".text") elf of
                   (s : _) -> return s
                   [] -> fail "Couldn't find .text section in the binary"
  return (elf, textSection)

loadBaseSet :: (U.HasLogCfg, WPO.OnlineSolver solver, sym ~ WEB.ExprBuilder t st fs)
            => [(Some (DPPC.Opcode DPPC.Operand), BS8.ByteString)]
            -> CBO.OnlineBackend solver t st fs
            -> IO (MapF.MapF (DPPC.Opcode DPPC.Operand) (F.ParameterizedFormula sym PPC32.PPC),
                   SemMC.SynthesisEnvironment sym PPC32.PPC)
loadBaseSet ops bak = do
  let sym = CRUB.backendGetSym bak
  env <- F.formulaEnv (Proxy @PPC32.PPC) sym
  baseSet <- F.loadFormulas sym (templateEnv env) F.emptyLibrary ops
  let plainBaseSet = makePlain baseSet
      synthEnv = SemMC.setupEnvironment bak baseSet
  return (plainBaseSet, synthEnv)
  where
    templateEnv :: F.FormulaEnv sym arch -> F.FormulaEnv sym (SemMC.TemplatedArch arch)
    templateEnv (F.FormulaEnv fs b) = F.FormulaEnv fs b


symbolicallyExecute
  :: (Architecture arch, Traversable t1, CRUB.IsSymBackend (WEB.ExprBuilder t2 st fs) bak)
  => bak
  -> MapF.MapF
       (SemMC.Architecture.Opcode arch (SemMC.Architecture.Operand arch))
       (F.ParameterizedFormula (WEB.ExprBuilder t2 st fs) arch)
  -> t1 (DPPC.GenericInstruction
           (SemMC.Architecture.Opcode arch) (SemMC.Architecture.Operand arch))
  -> IO (F.Formula (WEB.ExprBuilder t2 st fs) arch)
symbolicallyExecute bak plainBaseSet insns = do
  let sym = CRUB.backendGetSym bak
  formulas <- traverse (instantiateFormula' bak plainBaseSet) insns
  F.foldrM (F.sequenceFormulas sym) F.emptyFormula formulas

rewriteElfText :: E.ElfSection w -> E.Elf 32 -> [DPPC.Instruction] -> BSL.ByteString
rewriteElfText textSection elf newInsns =
  E.renderElf newElf
  where
    newInsnBytes = BSL.toStrict (foldMap DPPC.assembleInstruction newInsns)
    newElf = I.runIdentity (E.updateSections upd elf)
    upd sect
      | E.elfSectionIndex sect == E.elfSectionIndex textSection =
        pure (Just sect { E.elfSectionSize = fromIntegral (BS.length newInsnBytes)
                        , E.elfSectionData = newInsnBytes
                        })
      | otherwise = pure (Just sect)

printProgram :: [DPPC.Instruction] -> String
printProgram insns = unlines (Prelude.map (("  " ++) . show . DPPC.ppInstruction) insns)

main :: IO ()
main = do
  logCfg <- U.mkLogCfg "main"
  let ?logCfg = logCfg
  logThread <- U.asyncLinked (U.stdErrLogEventConsumer (const True) logCfg)
  N.withIONonceGenerator $ \r -> (O.execParser opts >>= mainWith r)
  U.logEndWith logCfg
  A.wait logThread
  where
    opts = O.info (O.helper <*> options) components
    components = mconcat [ O.fullDesc
                         , O.progDesc "A demo driver for program synthesis (PPC)"
                         , O.header "SynthDemo"
                         ]

data SemMCPPCData t = SemMCPPCData

mainWith :: (U.HasLogCfg) => N.NonceGenerator IO s -> Options -> IO ()
mainWith r opts = do
  -- Fetch the ELF from disk
  (elf, textSection) <- loadProgramBytes (oInputFile opts)
  let insnBytes = E.elfSectionData  textSection
  insns <- fromRightM (disassembleProgram insnBytes)

  -- Make it look nice
  putStrLn "This is the program you gave me, disassembled:"
  putStrLn (printProgram insns)

  -- Set up the synthesis side of things
  putStrLn ""
  putStrLn "Parsing semantics for known PPC opcodes"

  sym <- WEB.newExprBuilder WEB.FloatRealRepr SemMCPPCData r
  CBO.withYicesOnlineBackend sym CBO.NoUnsatFeatures WPF.noFeatures $ \bak -> do
    (plainBaseSet, synthEnv) <- loadBaseSet PPC32.allSemantics bak

    -- Turn it into a formula
    formula <- symbolicallyExecute bak plainBaseSet insns
    putStrLn ""
    putStrLn "Here's the formula for the whole program:"
    print formula

    -- Look for an equivalent program!
    putStrLn ""
    putStrLn "Starting synthesis..."
    let ?memOpts = LLVM.defaultMemOptions
    newInsns <- maybe (fail "Sorry, synthesis failed") return =<< SemMC.mcSynth bak synthEnv formula
    putStrLn ""
    putStrLn "Here's the equivalent program:"
    putStrLn (printProgram newInsns)

    -- Optionally append a return instruction so that we can call into this
    -- function.
    let retInsn = DPPC.Instruction DPPC.BLR DPPC.Nil
    newInsns' <- case oAppendReturn opts of
      False -> return newInsns
      True -> return (newInsns ++ [retInsn])
    let newObjBytes = rewriteElfText textSection elf newInsns'
    BSL.writeFile (oOutputFile opts) newObjBytes

    case oOriginalWithReturn opts of
      Nothing -> return ()
      Just owr -> do
        let origWithReturn = insns ++ [retInsn]
            origObjBytes = rewriteElfText textSection elf origWithReturn
        BSL.writeFile owr origObjBytes
