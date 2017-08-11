{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main ( main ) where

import           Control.Monad ( forever, when )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS8
import qualified Data.ByteString.Base16 as BSHex
import           Data.Functor.Identity ( runIdentity )
import           Data.Foldable ( foldrM, traverse_ )
import           System.Environment ( getArgs )
import           System.IO ( hFlush, stdout )
import           Text.Printf ( printf )

import           Data.ElfEdit ( findSectionByName, parseElf, ElfGetResult(..), ElfSection(..), updateSections, renderElf )

import           Data.Parameterized.Classes ( OrdF, ShowF(..) )
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Nonce ( newIONonceGenerator )
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.Witness ( Witness(..) )
import           Lang.Crucible.Solver.SimpleBackend ( newSimpleBackend )
import           Lang.Crucible.Solver.SimpleBuilder ( SimpleBuilder )

import qualified Dismantle.PPC as DPPC

import           SemMC.Architecture ( Architecture, Instruction, Location, Opcode, Operand )
import           SemMC.Formula ( emptyFormula, Formula, ParameterizedFormula )
import           SemMC.Formula.Instantiate ( instantiateFormula, sequenceFormulas )
import           SemMC.Synthesis.Template ( BaseSet, TemplatedArch, TemplatableOpcode, unTemplate )
import           SemMC.Synthesis ( mcSynth, setupEnvironment )

import qualified SemMC.Architecture.PPC as PPC

disassembleProgram :: BS.ByteString -> Either String [DPPC.Instruction]
disassembleProgram bs
  | BS.null bs = Right []
  | otherwise =
      case DPPC.disassembleInstruction (BSL.fromStrict bs) of
        (_lengthUsed, Just insn) ->
          -- FIXME: replace this "4" with lengthUsed once the Dismantle bug is fixed
          (insn :) <$> disassembleProgram (BS.drop 4 bs)
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
          -> MapF.MapF (Opcode arch (Operand arch)) (ParameterizedFormula sym arch)
makePlain = MapF.foldrWithKey f MapF.empty
  where f :: forall sh
           . TemplatableOpcode arch sh
          -> ParameterizedFormula sym (TemplatedArch arch) sh
          -> MapF.MapF (Opcode arch (Operand arch)) (ParameterizedFormula sym arch)
          -> MapF.MapF (Opcode arch (Operand arch)) (ParameterizedFormula sym arch)
        f (Witness op) pf = MapF.insert op (unTemplate pf)

instantiateFormula' :: (Architecture arch)
                    => SimpleBuilder t st
                    -> MapF.MapF (Opcode arch (Operand arch)) (ParameterizedFormula (SimpleBuilder t st) arch)
                    -> Instruction arch
                    -> IO (Formula (SimpleBuilder t st) arch)
instantiateFormula' sym m (DPPC.Instruction op params) =
  case MapF.lookup op m of
    Just pf -> snd <$> instantiateFormula sym pf params
    Nothing -> fail (printf "Couldn't find semantics for opcode \"%s\"" (showF op))

printLines :: (Traversable f, Show a) => f a -> IO ()
printLines = traverse_ (putStrLn . show)

main :: IO ()
main = do
  -- Fetch the ELF from disk
  objName <- head <$> getArgs
  objFile <- BS.readFile objName
  elf <- case parseElf objFile of
           Elf32Res _ e -> return e
           err -> fail "Expected an Elf32, but got, well, something else"
  textSection <- case findSectionByName (BS8.fromString ".text") elf of
                   (s : _) -> return s
                   [] -> fail "Couldn't find .text section in the binary"
  let insnBytes = elfSectionData textSection
  insns <- fromRightM (disassembleProgram insnBytes)

  -- Make it look nice
  putStrLn "This is the program you gave me, disassembled:"
  printLines insns

  -- Set up the synthesis side of things
  putStrLn ""
  putStrLn "Parsing semantics for known PPC opcodes"
  Some r <- newIONonceGenerator
  sym <- newSimpleBackend r
  baseSet <- PPC.loadBaseSet sym
  let plainBaseSet = makePlain baseSet
  synthEnv <- setupEnvironment sym baseSet

  -- Turn it into a formula
  forms <- traverse (instantiateFormula' sym plainBaseSet) insns
  form <- foldrM (sequenceFormulas sym) emptyFormula forms
  putStrLn ""
  putStrLn "Here's the formula for the whole program:"
  print form

  -- Look for an equivalent program!
  putStrLn ""
  putStrLn "Starting synthesis..."
  newInsns <- maybe (fail "Sorry, synthesis failed") return =<< mcSynth synthEnv form
  putStrLn ""
  putStrLn "Here's the equivalent program:"
  printLines newInsns

  let newInsnBytes = BSL.toStrict . mconcat $ map DPPC.assembleInstruction newInsns
      newElf = runIdentity $ updateSections upd elf
        where upd sect
                | elfSectionIndex sect == elfSectionIndex textSection =
                  pure . Just $ sect { elfSectionSize = fromIntegral (BS.length newInsnBytes)
                                     , elfSectionData = newInsnBytes
                                     }
                | otherwise = pure (Just sect)
      newObj = renderElf newElf

  BSL.writeFile (objName ++ ".out") newObj
