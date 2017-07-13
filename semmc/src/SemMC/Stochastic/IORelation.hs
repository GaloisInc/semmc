{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | A module for learning the input and output relations for instructions
module SemMC.Stochastic.IORelation (
  LearnConfig(..),
  IORelation(..),
  OperandRef(..),
  learn
  ) where

import Control.Applicative
import qualified Control.Concurrent as C
import qualified Control.Monad.Catch as E
import qualified Control.Monad.State.Strict as St
import Control.Monad.Trans ( MonadIO, liftIO )
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import Data.Proxy ( Proxy(..) )
import qualified Data.SCargot as SC
import qualified Data.SCargot.Repr as SC
import qualified Data.Text as T
import Data.Word ( Word64 )
import qualified Text.Parsec as P
import qualified Text.Parsec.Text as P

import qualified Data.Set.NonEmpty as NES
import Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.Map as MapF

import qualified Dismantle.Arbitrary as A
import qualified Dismantle.Instruction as D
import qualified Dismantle.Instruction.Random as D

import SemMC.Architecture
import qualified SemMC.Formula.Parser as F
import SemMC.Util ( Witness(..) )

import SemMC.Stochastic.Monad ( Sym )
import qualified SemMC.Stochastic.Remote as R

data LearnConfig t arch =
  LearnConfig { testChan :: C.Chan (Maybe (R.TestCase (ArchState (Sym t) arch)))
              , resChan :: C.Chan (R.ResultOrError (ArchState (Sym t) arch))
              , backend :: Sym t
              , testGen :: IO (ArchState (Sym t) arch)
              , gen :: A.Gen
              , assemble :: Instruction arch -> BS.ByteString
              , nonce :: !Word64
              -- ^ Nonces for test vectors
              }

data OperandRef arch sh = ImplicitOperand (Some (Location arch))
                        -- ^ A location that is implicitly read from or written to by an instruction
                        | forall s . OperandRef (D.Index sh s)
                        -- ^ An index into an operand list

data IORelation arch sh =
  IORelation { inputs :: [OperandRef arch sh]
             -- ^ Locations read by an instruction
             , outputs :: [OperandRef arch sh]
             -- ^ Locations written by an instruction
             }

newtype M t arch a = M { runM :: St.StateT (LearnConfig t arch) IO a }
  deriving (Functor,
            Applicative,
            Monad,
            St.MonadState (LearnConfig t arch),
            MonadIO)

-- | Find the locations read from and written to by each instruction passed in
--
-- This is determined by observing the behavior of instructions on tests and
-- perturbing inputs randomly.
learn :: (Architecture arch, R.MachineState (ArchState (Sym t) arch), D.ArbitraryOperands (Opcode arch) (Operand arch))
      => LearnConfig t arch
      -> [Some (Witness (F.BuildOperandList arch) (Opcode arch (Operand arch)))]
      -> IO (MapF.MapF (Opcode arch (Operand arch)) (IORelation arch))
learn config ops = St.evalStateT (runM act) config
  where
    act = F.foldlM (\m (Some (Witness op)) -> testOpcode m op) MapF.empty ops

printIORelation :: Opcode arch (Operand arch) sh -> IORelation arch sh -> T.Text
printIORelation = undefined

data IORelationParseError arch = IORelationParseError (Proxy arch) (Some (Opcode arch (Operand arch))) T.Text
                               | InvalidSExpr (Proxy arch) (SC.SExpr Atom)

deriving instance (Architecture arch) => Show (IORelationParseError arch)
instance (Architecture arch) => E.Exception (IORelationParseError arch)

readIORelation :: forall arch m sh
                . (E.MonadThrow m, Architecture arch)
               => Proxy arch
               -> T.Text
               -> Opcode arch (Operand arch) sh
               -> m (IORelation arch sh)
readIORelation p t op = do
  sx <- case parseLL t of
    Left err -> E.throwM (IORelationParseError p (Some op) t)
    Right res -> return res
  (inputsS, outputsS) <- case sx of
    SC.SCons (SC.SCons (SC.SAtom (AIdent "inputs")) inputsS)
             (SC.SCons (SC.SCons (SC.SAtom (AIdent "outputs")) outputsS)
                        SC.SNil) -> return (inputsS, outputsS)
    _ -> E.throwM (InvalidSExpr p sx)
  ins <- parseRelationList p op inputsS
  outs <- parseRelationList p op outputsS
  return IORelation { inputs = ins, outputs = outs }

parseRelationList :: (E.MonadThrow m)
                  => Proxy arch
                  -> Opcode arch (Operand arch) sh
                  -> SC.SExpr Atom
                  -> m [OperandRef arch sh]
parseRelationList = undefined

testOpcode :: (Architecture arch, R.MachineState (ArchState (Sym t) arch), D.ArbitraryOperands (Opcode arch) (Operand arch))
           => MapF.MapF (Opcode arch (Operand arch)) (IORelation arch)
           -> Opcode arch (Operand arch) sh
           -> M t arch (MapF.MapF (Opcode arch (Operand arch)) (IORelation arch))
testOpcode m op = do
  g <- St.gets gen
  mkTest <- St.gets testGen
  t0 <- liftIO mkTest
  insn <- liftIO $ D.randomInstruction g (NES.singleton (Some op))
  tests <- generateTestVariants insn t0
  tests' <- mapM (makeTestCase insn) tests
  tchan <- St.gets testChan
  liftIO $ mapM_ (C.writeChan tchan . Just) tests'
  return undefined

makeTestCase :: (Architecture arch, R.MachineState (ArchState (Sym t) arch))
             => Instruction arch
             -> ArchState (Sym t) arch
             -> M t arch (R.TestCase (ArchState (Sym t) arch))
makeTestCase i c = do
  tid <- St.gets nonce
  asm <- St.gets assemble
  St.modify' $ \s -> s { nonce = nonce s + 1 }
  return R.TestCase { R.testNonce = tid
                    , R.testContext = c
                    , R.testProgram = asm i
                    }

-- | Given an initial test state, generate all interesting variants on it.  The
-- idea is to see which outputs change when we tweak an input.
--
-- We learn the *inputs* set by starting with an initial test t0 and tweaking
-- each element in the state in turn.  For each tweaked input, we examine the
-- effects on the output states.  We want to avoid tweaking the registers that
-- are instantiated as operands to the instruction, as we expect those to cause
-- changes.  We really just need to learn which of the operands are inputs vs
-- outputs, and if there are any implicit arguments.
--
-- We learn the *outputs* set by comparing the tweaked input vs the output from
-- that test vector: all modified registers are in the output set.
generateTestVariants :: Instruction arch -> ArchState (Sym t) arch -> M t arch [ArchState (Sym t) arch]
generateTestVariants = undefined

-- On-disk data format (s-expression based)

{-

The format is expected to be a simple s-expression recording inputs and outputs

((inputs ((implicit . rax) (operand  . 0)))
 (outputs ()))

-}

data Atom = AIdent String
          | AWord Word
          deriving (Show)

parseIdent :: P.Parser String
parseIdent = undefined

parseWord :: P.Parser Word
parseWord = undefined

parseAtom :: P.Parser Atom
parseAtom = AIdent <$> parseIdent
        <|> AWord <$> parseWord

parserLL :: SC.SExprParser Atom (SC.SExpr Atom)
parserLL = SC.mkParser parseAtom

parseLL :: T.Text -> Either String (SC.SExpr Atom)
parseLL = SC.decodeOne parserLL

{-
class BuildShapedList (tps :: [k]) where
  buildShapedList :: forall tp m a b c
                   . (E.MonadThrow m)
                  => a
                  -> (a -> m (b '[]))
                  -- ^ The function to call on an empty list
                  -> (a -> m (c tp, a))
                  -- ^ The function to call on the seed to split it into a value
                  -- (to be combined into a larger result) and the rest of the
                  -- seed (to be consumed later)
                  -> (c tp -> b tps -> m (b (tp ': tps)))
                  -- ^ The function to combine the current element with the rest
                  -- of the constructed shape
                  -> m (b tps)

instance BuildShapedList '[] where
  buildShapedList a nil _f _c = nil a

instance BuildShapedList (tp ': tps) where
  buildShapedList a nil f c = do
    (elt, rest) <- f a
    rest' <- buildShapedList rest nil f c
    c elt rest'
-}
