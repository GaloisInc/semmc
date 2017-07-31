{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
module SemMC.Stochastic.IORelation.Parser (
  readIORelation,
  printIORelation
  ) where

import Control.Applicative
import qualified Control.Monad.Catch as E
import Data.Proxy ( Proxy(..) )
import qualified Data.SCargot as SC
import qualified Data.SCargot.Repr as SC
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Text as P
import Text.Read ( readMaybe )

import Data.Parameterized.Some ( Some(..) )
import qualified Dismantle.Instruction as D

import qualified Data.Parameterized.Unfold as U
import SemMC.Architecture
import qualified SemMC.ConcreteState as CS
import SemMC.Stochastic.IORelation.Types

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

printIORelation :: forall arch sh . (CS.ConcreteArchitecture arch) => IORelation arch sh -> T.Text
printIORelation = SC.encodeOne (SC.basicPrint printAtom) . (fromIORelation (Proxy :: Proxy arch))

printAtom :: Atom -> T.Text
printAtom a =
  case a of
    AIdent s -> T.pack s
    AWord w -> T.pack (show w)

fromIORelation :: (CS.ConcreteArchitecture arch) => Proxy arch -> IORelation arch sh -> SC.SExpr Atom
fromIORelation p ior =
  SC.SCons (SC.SCons (SC.SAtom (AIdent "inputs")) inputsS)
           (SC.SCons (SC.SCons (SC.SAtom (AIdent "outputs")) outputsS)
                      SC.SNil)
  where
    inputsS = fromList (map toSExpr (S.toList (inputs ior)))
    outputsS = fromList (map toSExpr (S.toList (outputs ior)))

    fromList = foldr SC.SCons SC.SNil

    toSExpr rel =
      case rel of
        ImplicitOperand (Some loc) -> SC.SAtom (AIdent (CS.showView loc))
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
                . (E.MonadThrow m, CS.ConcreteArchitecture arch, U.UnfoldShape sh)
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
  return IORelation { inputs = S.fromList ins, outputs = S.fromList outs }

parseRelationList :: forall m sh arch
                   . (E.MonadThrow m, U.UnfoldShape sh, CS.ConcreteArchitecture arch)
                  => Proxy arch
                  -> Opcode arch (Operand arch) sh
                  -> SC.SExpr Atom
                  -> m [OperandRef arch sh]
parseRelationList proxy opcode s0 =
  case s0 of
    SC.SNil -> return []
    SC.SCons (SC.SCons (SC.SAtom (AIdent "implicit")) (SC.SAtom (AIdent loc))) rest -> do
      rest' <- parseRelationList proxy opcode rest
      case CS.readView loc of
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

