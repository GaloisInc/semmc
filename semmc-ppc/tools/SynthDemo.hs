{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main ( main ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS8
import qualified Data.ByteString.Base16 as BSHex
import qualified Data.Functor.Identity as I
import qualified Data.Foldable as F
import           Data.Monoid
import           Data.Word ( Word32 )
import qualified Options.Applicative as O
import           Text.Printf ( printf )

import qualified Data.ElfEdit as E

import           Data.Parameterized.Classes ( OrdF, ShowF(..) )
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as N
import           Data.Parameterized.Witness ( Witness(..) )
import qualified Lang.Crucible.Solver.SimpleBackend as SB
import qualified Lang.Crucible.Solver.SimpleBuilder as SB

import qualified Dismantle.PPC as DPPC

import           SemMC.Architecture ( Architecture, Instruction, Location, Opcode, Operand )
import qualified SemMC.Formula as F
import qualified SemMC.Formula.Instantiate as F
import           SemMC.Synthesis.Template ( BaseSet, TemplatedArch, TemplatableOpcode, unTemplate )
import qualified SemMC.Synthesis as SemMC
import qualified SemMC.Synthesis.Core as SemMC

import qualified SemMC.Architecture.PPC as PPC

data Options = Options { oInputFile :: FilePath
                       , oOutputFile :: FilePath
                       }

options :: O.Parser Options
options = Options <$> O.strArgument ( O.metavar "FILE"
                                    <> O.help "An object file to use as a specification" )
                  <*> O.strOption ( O.long "output"
                                  <> O.short 'o'
                                  <> O.metavar "FILE"
                                  <> O.help "The file to write out" )

disassembleProgram :: BS.ByteString -> Either String [DPPC.Instruction]
disassembleProgram bs
  | BS.null bs = Right []
  | otherwise =
      case DPPC.disassembleInstruction (BSL.fromStrict bs) of
        (lengthUsed, Just insn) ->
          -- FIXME: replace this "4" with lengthUsed once the Dismantle bug is fixed
          (insn :) <$> disassembleProgram (BS.drop lengthUsed bs)
        (lengthUsed, Nothing) ->
          let badInsnHex = BS8.toString (BSHex.encode (BS.take lengthUsed bs))
          in Left (printf "Invalid instruction \"%s\"" badInsnHex)

fromRightM :: (Monad m) => Either String a -> m a
fromRightM (Left err) = fail err
fromRightM (Right val) = return val

makePlain :: forall arch sym
           . (OrdF (Opcode arch (Operand arch)),
              OrdF (Location arch))
          => BaseSet sym arch
          -> MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula sym arch)
makePlain = MapF.foldrWithKey f MapF.empty
  where f :: forall sh
           . TemplatableOpcode arch sh
          -> F.ParameterizedFormula sym (TemplatedArch arch) sh
          -> MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula sym arch)
          -> MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula sym arch)
        f (Witness op) pf = MapF.insert op (unTemplate pf)

instantiateFormula' :: (Architecture arch)
                    => SB.SimpleBuilder t st
                    -> MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula (SB.SimpleBuilder t st) arch)
                    -> Instruction arch
                    -> IO (F.Formula (SB.SimpleBuilder t st) arch)
instantiateFormula' sym m (DPPC.Instruction op params) =
  case MapF.lookup op m of
    Just pf -> snd <$> F.instantiateFormula sym pf params
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

loadBaseSet :: SB.SimpleBuilder t SB.SimpleBackendState
            -> IO (MapF.MapF (DPPC.Opcode DPPC.Operand) (F.ParameterizedFormula (SB.SimpleBuilder t SB.SimpleBackendState) PPC.PPC),
                   SemMC.SynthesisEnvironment (SB.SimpleBackend t) PPC.PPC)
loadBaseSet sym = do
  baseSet <- PPC.loadBaseSet sym
  let plainBaseSet = makePlain baseSet
  synthEnv <- SemMC.setupEnvironment sym baseSet
  return (plainBaseSet, synthEnv)

symbolicallyExecute :: (Architecture arch, Traversable t)
                    => SB.SimpleBuilder s st
                    -> MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula (SB.SimpleBuilder s st) arch)
                    -> t (DPPC.GenericInstruction (Opcode arch) (Operand arch))
                    -> IO (F.Formula (SB.SimpleBuilder s st) arch)
symbolicallyExecute sym plainBaseSet insns = do
  formulas <- traverse (instantiateFormula' sym plainBaseSet) insns
  F.foldrM (F.sequenceFormulas sym) F.emptyFormula formulas

rewriteElfText :: E.ElfSection w -> E.Elf 32 -> [DPPC.Instruction] -> BSL.ByteString
rewriteElfText textSection elf newInsns =
  E.renderElf newElf
  where
    newInsnBytes = BSL.toStrict (mconcat (map DPPC.assembleInstruction newInsns))
    newElf = I.runIdentity (E.updateSections upd elf)
    upd sect
      | E.elfSectionIndex sect == E.elfSectionIndex textSection =
        pure (Just sect { E.elfSectionSize = fromIntegral (BS.length newInsnBytes)
                        , E.elfSectionData = newInsnBytes
                        })
      | otherwise = pure (Just sect)

printProgram :: [DPPC.Instruction] -> String
printProgram insns = unlines (map (("  " ++) . show . DPPC.ppInstruction) insns)

main :: IO ()
main = N.withIONonceGenerator $ \r -> (O.execParser opts >>= mainWith r)
  where
    opts = O.info (O.helper <*> options) components
    components = mconcat [ O.fullDesc
                         , O.progDesc "A demo driver for program synthesis (PPC)"
                         , O.header "SynthDemo"
                         ]

mainWith :: N.NonceGenerator IO s -> Options -> IO ()
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
  sym <- SB.newSimpleBackend r
  (plainBaseSet, synthEnv) <- loadBaseSet sym

  -- Turn it into a formula
  formula <- symbolicallyExecute sym plainBaseSet insns
  putStrLn ""
  putStrLn "Here's the formula for the whole program:"
  print formula

  -- Look for an equivalent program!
  putStrLn ""
  putStrLn "Starting synthesis..."
  newInsns <- maybe (fail "Sorry, synthesis failed") return =<< SemMC.mcSynth synthEnv formula
  putStrLn ""
  putStrLn "Here's the equivalent program:"
  putStrLn (printProgram newInsns)

  let newObjBytes = rewriteElfText textSection elf newInsns
  BSL.writeFile (oOutputFile opts) newObjBytes
