{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
-- | A module for learning the input and output relations for instructions
module SemMC.Stochastic.IORelation (
  LearnConfig(..),
  IORelation(..),
  OperandRef(..),
  learn,
  readIORelation,
  printIORelation
  ) where

import Control.Applicative
import qualified Control.Concurrent as C
import Control.Monad ( replicateM )
import qualified Control.Monad.Catch as E
import qualified Control.Monad.State.Strict as St
import Control.Monad.Trans ( MonadIO, liftIO )
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import Data.Proxy ( Proxy(..) )
import qualified Data.SCargot as SC
import qualified Data.SCargot.Repr as SC
import qualified Data.Text as T
import Data.Typeable ( Typeable )
import Data.Word ( Word64 )
import System.Timeout ( timeout )
import qualified Text.Parsec as P
import qualified Text.Parsec.Text as P
import Text.Read ( readMaybe )

import qualified Data.Set.NonEmpty as NES
import Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.Map as MapF
import qualified Lang.Crucible.BaseTypes as S
import qualified Lang.Crucible.Solver.Interface as S

import qualified Dismantle.Arbitrary as A
import qualified Dismantle.Instruction as D
import qualified Dismantle.Instruction.Random as D

import qualified Data.Parameterized.Unfold as U
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
              , resWaitSeconds :: Int
              -- ^ Number of seconds to wait to receive all of the results over the 'resChan'
              }

data OperandRef arch sh = ImplicitOperand (Some (Location arch))
                        -- ^ A location that is implicitly read from or written to by an instruction
                        | OperandRef (Some (D.Index sh))
                        -- ^ An index into an operand list

deriving instance (Architecture arch) => Eq (OperandRef arch sh)
deriving instance (Architecture arch) => Ord (OperandRef arch sh)

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

-- | Number of microseconds to wait for all results to come in over the channel
askWaitMicroseconds :: M t arch Int
askWaitMicroseconds = (* 1000000) <$> St.gets resWaitSeconds

data LearningException arch = LearningTimeout (Proxy arch) (Some (Opcode arch (Operand arch)))

deriving instance (Architecture arch) => Show (LearningException arch)

instance (Architecture arch, Typeable arch) => E.Exception (LearningException arch)

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

testOpcode :: forall arch sh t . (Architecture arch, R.MachineState (ArchState (Sym t) arch), D.ArbitraryOperands (Opcode arch) (Operand arch))
           => MapF.MapF (Opcode arch (Operand arch)) (IORelation arch)
           -> Opcode arch (Operand arch) sh
           -> M t arch (MapF.MapF (Opcode arch (Operand arch)) (IORelation arch))
testOpcode m op = do
  implicitOperands <- findImplicitOperands op
  explicitOperands <- classifyExplicitOperands (Proxy :: Proxy arch) op implicitOperands
  let ioRelation = mergeIORelations implicitOperands explicitOperands
  return $ MapF.insert op ioRelation m

mergeIORelations :: IORelation arch sh -> IORelation arch sh -> IORelation arch sh
mergeIORelations ior1 ior2 =
  IORelation { inputs = inputs ior1 ++ inputs ior2
             , outputs = outputs ior1 ++ outputs ior2
             }

-- | Collect all of the locations that are read from or written to implicitly
implicitLocations :: IORelation arch sh -> [Some (Location arch)]
implicitLocations ior = foldr collectImplicits (foldr collectImplicits [] (inputs ior)) (outputs ior)
  where
    collectImplicits opRef acc =
      case opRef of
        ImplicitOperand sloc -> sloc : acc
        OperandRef {} -> acc

-- | Make a random instruction that does not reference any implicit operands.
--
-- This could be made more efficient - right now, it just tries to generate
-- random instructions until it gets a match.
generateExplicitInstruction :: (Architecture arch, D.ArbitraryOperands (Opcode arch) (Operand arch))
                            => Proxy arch
                            -> Opcode arch (Operand arch) sh
                            -> [Some (Location arch)]
                            -> M t arch (Instruction arch)
generateExplicitInstruction proxy op implicitOperands = do
  g <- St.gets gen
  insn <- liftIO $ D.randomInstruction g (NES.singleton (Some op))
  case insn of
    D.Instruction _ ops ->
      case D.foldrOperandList (matchesOperand proxy implicitOperands) False ops of
        True -> generateExplicitInstruction proxy op implicitOperands
        False -> return insn

matchesOperand :: (Architecture arch)
               => Proxy arch
               -> [Some (Location arch)]
               -> D.Index sh tp
               -> Operand arch tp
               -> Bool
               -> Bool
matchesOperand proxy implicits _ix operand matches =
  case operandToLocation proxy operand of
    Nothing -> matches
    Just loc -> matches || any (== Some loc) implicits

classifyExplicitOperands :: (Architecture arch, R.MachineState (ArchState (Sym t) arch), D.ArbitraryOperands (Opcode arch) (Operand arch))
                         => Proxy arch
                         -> Opcode arch (Operand arch) sh
                         -> IORelation arch sh
                         -> M t arch (IORelation arch sh)
classifyExplicitOperands proxy op (implicitLocations -> implicitOperands) = do
  mkTest <- St.gets testGen
  t0 <- liftIO mkTest
  insn <- generateExplicitInstruction proxy op implicitOperands
  tests <- generateTestVariants proxy implicitOperands insn t0
  tests' <- mapM (wrapTestBundle insn) tests
  tchan <- St.gets testChan
  let remoteTestCases = [ t
                        | tb <- tests'
                        , t <- tbTestOrig tb : tbTestCases tb
                        ]
  liftIO $ mapM_ (C.writeChan tchan . Just) remoteTestCases
  ms <- askWaitMicroseconds
  rchan <- St.gets resChan
  mresults <- liftIO $ timeout ms $ replicateM (length remoteTestCases) (C.readChan rchan)
  case mresults of
    Just results -> computeIORelation op tests' results
    Nothing -> liftIO $ E.throwM (LearningTimeout proxy (Some op))

-- | Sweep through the parameter space to find locations not mentioned in
-- parameter lists that are modified by the instruction.
--
-- To do this, we generate a bunch of randomized operand lists to cycle through
-- possible registers.
findImplicitOperands :: (Architecture arch)
                     => Opcode arch (Operand arch) sh
                     -> M t arch (IORelation arch sh)
findImplicitOperands = undefined

-- | Given a bundle of tests, wrap all of the contained raw test cases with nonces.
wrapTestBundle :: (Architecture arch, R.MachineState (ArchState (Sym t) arch))
               => Instruction arch
               -> TestBundle (ArchState (Sym t) arch) arch
               -> M t arch (TestBundle (R.TestCase (ArchState (Sym t) arch)) arch)
wrapTestBundle i tb = do
  orig <- makeTestCase i (tbTestOrig tb)
  cases <- mapM (makeTestCase i) (tbTestCases tb)
  return TestBundle { tbTestOrig = orig
                    , tbTestCases = cases
                    , tbResult = tbResult tb
                    }

-- | Take a test bundle of raw tests ('ArchState (Sym t) arch') and convert the
-- raw tests to 'R.TestCase' by allocating a nonce
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

-- | Run tests to determine the input and output locations for the given instruction opcode.
--
-- 1) Learn implicit operands by choosing a large set of randomly chosen
-- operands: operands that change when not present in the operand list are
-- implicit
--
-- 2) Generate a random instruction that contains no explicit references to
-- implicit operands.  Then generate test variants to determine if each
-- referenced location is an input or an output.
computeIORelation :: (Architecture arch)
                  => Opcode arch (Operand arch) sh
                  -> [TestBundle (R.TestCase (ArchState (Sym t) arch)) arch]
                  -> [R.ResultOrError (ArchState (Sym t) arch)]
                  -> M t arch (IORelation arch sh)
computeIORelation = undefined

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
generateTestVariants :: forall proxy arch t
                      . (Architecture arch)
                     => proxy arch
                     -> [Some (Location arch)]
                     -> Instruction arch
                     -> ArchState (Sym t) arch
                     -> M t arch [TestBundle (ArchState (Sym t) arch) arch]
generateTestVariants proxy implicitOperands i s0 =
  case i of
    D.Instruction opcode operands -> do
      mapM (genVar opcode) (instructionRegisterOperands proxy operands)
  where
    genVar :: forall sh
            . Opcode arch (Operand arch) sh
           -> Some (PairF (D.Index sh) (TypedLocation arch))
           -> M t arch (TestBundle (ArchState (Sym t) arch) arch)
    genVar opcode (Some (PairF ix (TL loc))) = do
      cases <- generateVariantsFor proxy s0 opcode ix loc
      return TestBundle { tbTestOrig = s0
                        , tbTestCases = cases
                        , tbResult = Learned { lOpcode = opcode
                                             , lIndex = ix
                                             , lLocation = loc
                                             }
                        }

-- FIXME: For each test variant, build a new structure that tells us what we
-- learn if there is a difference from the original.  We'll need to map those to
-- nonces to compare against the results we get back.
--
-- To learn implicit operands, we need the list of all (register) locations for
-- the architecture.  We won't deal with implicit memory locations

data TestBundle f arch = TestBundle { tbTestOrig :: f
                                    -- ^ The base case to compare against
                                    , tbTestCases :: [f]
                                    -- ^ The variants to run
                                    , tbResult :: LearnedFact arch
                                    -- ^ The fact we learn if the test cases
                                    -- differ
                                    }

-- | If the given location changes, it was an output location.  Otherwise, if
-- the test cases differ from the original test case, it was an input operand.
data LearnedFact arch =
  forall sh tp . Learned { lOpcode :: Opcode arch (Operand arch) sh
                         , lIndex :: D.Index sh tp
                         , lLocation :: Location arch (OperandType arch tp)
                         }

-- | Tweak the value in the 'ArchState' at the given location to a number of
-- random values.
--
-- This has to be in IO so that we can generate 'S.SymExpr's
--
-- Right now, we only support generating random bitvectors.  That will get more
-- interesting once we start dealing with floats.  Note that we could just
-- generate random bitvectors for floats, too, but we would probably want to
-- tweak the distribution to generate interesting types of floats.
generateVariantsFor :: (Architecture arch)
                    => proxy arch
                    -> ArchState (Sym t) arch
                    -> Opcode arch (Operand arch) sh
                    -> D.Index sh tp
                    -> Location arch (OperandType arch tp)
                    -> M t arch [ArchState (Sym t) arch]
generateVariantsFor _ s0 opcode _ix loc = do
  sym <- St.gets backend
  g <- St.gets gen
  replicateM 5 (genOne sym g)
  where
    genOne sym g =
      case locationType loc of
        S.BaseBVRepr w -> do
          randomInt :: Int
                    <- liftIO (A.uniform g)
          bv <- liftIO $ S.bvLit sym w (fromIntegral randomInt)
          return (MapF.insert loc bv s0)
        repr -> error ("Unsupported base type repr in generateVariantsFor: " ++ show repr)

-- | This is a newtype to shuffle type arguments around so that the 'tp'
-- parameter is last (so that we can use it with PairF and Some)
newtype TypedLocation arch tp = TL (Location arch (OperandType arch tp))

data PairF a b tp = PairF (a tp) (b tp)

instructionRegisterOperands :: forall arch sh proxy
                             . (Architecture arch)
                            => proxy arch
                            -> D.OperandList (Operand arch) sh
                            -> [Some (PairF (D.Index sh) (TypedLocation arch))]
instructionRegisterOperands proxy operands =
  D.foldrOperandList collectLocations [] operands
  where
    collectLocations :: forall tp . D.Index sh tp
                     -> Operand arch tp
                     -> [Some (PairF (D.Index sh) (TypedLocation arch))]
                     -> [Some (PairF (D.Index sh) (TypedLocation arch))]
    collectLocations ix operand acc =
      case operandToLocation proxy operand of
        Just loc -> Some (PairF ix (TL loc)) : acc
        Nothing -> acc

{-

We want to generate tests to determine, for each register operand, if it is
input, output, or both.

We'll start off with a single initial register state passed to
generateTestVariants.  All of the variants will be derived from that state.

We need to walk down the operand list and, for each register operand (r0),
generate a set of new states with that register (r0) value tweaked.  For those
nonces, if other registers change in the post state, r0 was an input register.
Registers that change values in the post state are outputs.  If registers that
are not mentioned in the operand list change, they are implicit outputs.  If
changing a register not in the operand list causes a change in outputs, it is an
implicit input.


-}

-- On-disk data format (s-expression based)

{-

The format is expected to be a simple s-expression recording inputs and outputs

((inputs ((implicit . rax) (operand . 0)))
 (outputs ()))

-}

data Atom = AIdent String
          | AWord Word
          deriving (Show)

parseIdent :: P.Parser String
parseIdent = P.many P.letter

parseWord :: P.Parser Word
parseWord = do
  mw <- P.many1 P.digit
  case readMaybe mw of
    Just w -> return w
    Nothing -> fail "Invalid word"

parseAtom :: P.Parser Atom
parseAtom = AIdent <$> parseIdent
        <|> AWord <$> parseWord

parserLL :: SC.SExprParser Atom (SC.SExpr Atom)
parserLL = SC.mkParser parseAtom

parseLL :: T.Text -> Either String (SC.SExpr Atom)
parseLL = SC.decodeOne parserLL

printIORelation :: forall arch sh . (Architecture arch) => IORelation arch sh -> T.Text
printIORelation = SC.encodeOne (SC.basicPrint printAtom) . (fromIORelation (Proxy :: Proxy arch))

printAtom :: Atom -> T.Text
printAtom a =
  case a of
    AIdent s -> T.pack s
    AWord w -> T.pack (show w)

fromIORelation :: (Architecture arch) => Proxy arch -> IORelation arch sh -> SC.SExpr Atom
fromIORelation p ior =
  SC.SCons (SC.SCons (SC.SAtom (AIdent "inputs")) inputsS)
           (SC.SCons (SC.SCons (SC.SAtom (AIdent "outputs")) outputsS)
                      SC.SNil)
  where
    inputsS = fromList (map toSExpr (inputs ior))
    outputsS = fromList (map toSExpr (outputs ior))

    fromList = foldr SC.SCons SC.SNil

    toSExpr rel =
      case rel of
        ImplicitOperand loc -> SC.SAtom (AIdent (show loc))
        OperandRef (Some ix) -> SC.SAtom (AWord (indexToWord p ix))

indexToWord :: Proxy arch -> D.Index sh s -> Word
indexToWord p ix =
  case ix of
    D.IndexHere -> 0
    D.IndexThere ix' -> 1 + indexToWord p ix'

data IORelationParseError arch = IORelationParseError (Proxy arch) (Some (Opcode arch (Operand arch))) T.Text
                               | InvalidSExpr (Proxy arch) (Some (Opcode arch (Operand arch))) (SC.SExpr Atom)
                               | InvalidLocation (Proxy arch) String
                               | InvalidIndex (Proxy arch) (Some (Opcode arch (Operand arch))) Word

deriving instance (Architecture arch) => Show (IORelationParseError arch)
instance (Architecture arch) => E.Exception (IORelationParseError arch)

readIORelation :: forall arch m sh
                . (E.MonadThrow m, Architecture arch, U.UnfoldShape sh)
               => Proxy arch
               -> T.Text
               -> Opcode arch (Operand arch) sh
               -> m (IORelation arch sh)
readIORelation p t op = do
  sx <- case parseLL t of
    Left _err -> E.throwM (IORelationParseError p (Some op) t)
    Right res -> return res
  (inputsS, outputsS) <- case sx of
    SC.SCons (SC.SCons (SC.SAtom (AIdent "inputs")) inputsS)
             (SC.SCons (SC.SCons (SC.SAtom (AIdent "outputs")) outputsS)
                        SC.SNil) -> return (inputsS, outputsS)
    _ -> E.throwM (InvalidSExpr p (Some op) sx)
  ins <- parseRelationList p op inputsS
  outs <- parseRelationList p op outputsS
  return IORelation { inputs = ins, outputs = outs }

parseRelationList :: forall m sh arch
                   . (E.MonadThrow m, U.UnfoldShape sh, Architecture arch)
                  => Proxy arch
                  -> Opcode arch (Operand arch) sh
                  -> SC.SExpr Atom
                  -> m [OperandRef arch sh]
parseRelationList proxy opcode s0 =
  case s0 of
    SC.SNil -> return []
    SC.SCons (SC.SCons (SC.SAtom (AIdent "implicit")) (SC.SAtom (AIdent loc))) rest -> do
      rest' <- parseRelationList proxy opcode rest
      case readLocation loc of
        Nothing -> E.throwM (InvalidLocation proxy loc)
        Just sloc -> return (ImplicitOperand sloc : rest')
    SC.SCons (SC.SCons (SC.SAtom (AIdent "operand")) (SC.SAtom (AWord ix))) rest -> do
      rest' <- parseRelationList proxy opcode rest
      oref <- mkOperandRef proxy opcode ix
      return (oref : rest')
    _ -> E.throwM (InvalidSExpr proxy (Some opcode) s0)

-- | Take an integer and try to construct a `D.Index` that points at the
-- appropriate index into the operand list of the given opcode.
--
-- This involves traversing the type level operand list via 'U.unfoldShape'
mkOperandRef :: forall m arch sh . (E.MonadThrow m, U.UnfoldShape sh, Architecture arch)
             => Proxy arch
             -> Opcode arch (Operand arch) sh
             -> Word
             -> m (OperandRef arch sh)
mkOperandRef proxy op w0 = U.unfoldShape nil elt w0
  where
    -- We got to the end of the type level list without finding our index, so it
    -- was out of bounds
    nil :: Word -> m (OperandRef arch '[])
    nil _ = E.throwM (InvalidIndex proxy (Some op) w0)

    elt :: forall tp tps' tps . (U.RecShape tp tps' tps) => Proxy tp -> Proxy tps' -> Word -> m (OperandRef arch tps)
    elt _ _ w =
      case w of
        0 -> return (OperandRef (Some D.IndexHere))
        _ -> do
          OperandRef (Some ix) <- U.unfoldShape nil elt (w - 1)
          return (OperandRef (Some (D.IndexThere ix)))

