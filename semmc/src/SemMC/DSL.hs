{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | A DSL to help defining instruction semantics to populate the base set (and manual set)
module SemMC.DSL (
  -- * Definitions
  defineOpcode,
  param,
  input,
  inputLiteral,
  defLoc,
  comment,
  -- * Operations
  extract,
  zeroExtend,
  signExtend,
  concat,
  ite,
  uf,
  -- ** Arithmetic bitvector ops
  bvadd,
  bvsub,
  bvmul,
  -- ** Bitwise bitvector ops
  bvxor,
  bvor,
  bvand,
  bvshl,
  bvlshr,
  bvnot,
  -- ** Bitwise bitvector comparisons
  bvule,
  bvult,
  bvuge,
  bvugt,
  bvsle,
  bvslt,
  bvsge,
  bvsgt,
  -- * Expressions
  Expr(..),
  ExprTag(..),
  ExprType(..),
  exprType,
  Location(..),
  -- * Monad
  SemM,
  Phase(..),
  runSem,
  Parameter,
  Definition,
  printDefinition
  ) where

import           GHC.Stack ( HasCallStack )

import           Prelude hiding ( concat )

import qualified Control.Monad.RWS.Strict as RWS
import qualified Data.Foldable as F
import           Data.Monoid
import qualified Data.SCargot as SC
import qualified Data.SCargot.Repr as SC
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Text.Printf ( printf )

import           Data.Parameterized.Classes ( ShowF(..) )
import           Data.Parameterized.Some ( Some(..) )

data ExprTag = TBool
              | TBV
              | TInt
              | TFloat
              | TDouble

data ExprType = EBV Int
              -- ^ Bit vector with width
              | EInt
              -- ^ Integer
              | EFloat
              -- ^ Float
              | EDouble
              -- ^ Double
              | EBool
              deriving (Eq, Show)

-- | An expression representing an SMT formula.  It can reference parameters
--
-- Note that there are some GADT type tags -- unlike crucible, we never need to
-- recover those.  They are only there to guard term construction.
data Expr (tp :: ExprTag) where
  LitBV :: Int -> Integer -> Expr 'TBV
  LitInt :: Integer -> Expr 'TInt
  Loc :: ExprType -> String -> Expr tp
  Param :: ExprType -> Parameter -> Expr tp
  -- | Built-in operations (e.g., bitvector ops)
  Builtin :: ExprType -> String -> [Some Expr] -> Expr tp
  -- | Functions provided by theory backends that are called with the underscore
  -- syntax in smt (e.g., extract and extend)
  TheoryFunc :: ExprType -> String -> [Some Expr] -> [Some Expr] -> Expr tp
  -- | User-defined uninterpreted functions called with the @call@ SMTLib
  -- primitive
  UninterpretedFunc :: ExprType -> String -> [Some Expr] -> Expr tp

deriving instance Show (Expr tp)

exprType :: Expr tp -> ExprType
exprType e =
  case e of
    LitBV w _ -> EBV w
    LitInt _ -> EInt
    Loc t _ -> t
    Param t _ -> t
    Builtin t _ _ -> t
    TheoryFunc t _ _ _ -> t
    UninterpretedFunc t _ _ -> t

-- | A parameter and its type
--
-- The type is a string corresponding to an operand type from the architecture
-- (e.g., Gprc), rather than an 'ExprType'.
data Parameter = Parameter { pName :: String
                           , pType :: String
                           }
               deriving (Show)

data Location = ParamLoc Parameter
              | LiteralLoc String
              deriving (Show)

data Formula = Formula { fName :: String
                       , fOperands :: Seq.Seq Parameter
                       , fInputs :: [Location]
                       , fDefs :: [(Location, Some Expr)]
                       , fComment :: Seq.Seq String
                       -- ^ Comments stored as individual lines
                       }
             deriving (Show)

instance ShowF Expr

-- | The state component of the monad is a Formula that is built up during a
-- single definition; after the definition, it is added to the output sequence.
--
-- The @t@ is a phantom parameter to ensure that nesting definitions is
-- impossible.
newtype SemM (t :: Phase) a = SemM { unSem :: RWS.RWS () (Seq.Seq Formula) Formula a }
  deriving (Functor,
            Applicative,
            Monad,
            RWS.MonadWriter (Seq.Seq Formula),
            RWS.MonadState Formula)

-- | Tags used as phantom types to prevent nested opcode definitions
data Phase = Top | Def

data Definition = Definition (Seq.Seq String) (SC.SExpr FAtom)
  deriving (Show)

-- | Run a semantics defining action and return the defined formulas.
--
-- The result is an association list from opcode name to the s-expression
-- representing it.
runSem :: SemM 'Top () -> [(String, Definition)]
runSem act = mkSExprs (snd (RWS.execRWS (unSem act) () badFormula))
  where
    -- This is a dummy formula that is never used.  It is just a standin until
    -- the first call to 'defineOpcode'.  If 'defineOpcode' is never called,
    -- this will never be used since 'defineOpcode' handles adding the result to
    -- the writer output.
    badFormula = Formula { fName = ""
                         , fComment = Seq.empty
                         , fOperands = Seq.empty
                         , fInputs = []
                         , fDefs = []
                         }

-- | Define an opcode with a given name.
--
-- The body is executed to produce a definition.
defineOpcode :: String -> SemM 'Def () -> SemM 'Top ()
defineOpcode name (SemM def) = do
  let freshFormula = Formula { fName = name
                             , fComment = Seq.empty
                             , fOperands = Seq.empty
                             , fInputs = []
                             , fDefs = []
                             }
  RWS.put freshFormula
  SemM def
  newFormula <- RWS.get
  RWS.tell (Seq.singleton newFormula)
  return ()

-- | Add a descriptive comment to the output file
--
-- Each call appends a new comment line.  Individual calls to comment should not
-- contain newlines.
comment :: String -> SemM 'Def ()
comment c = RWS.modify' $ \f -> f { fComment = fComment f Seq.|> c }

-- | Declare a named parameter; the string provided is used in the produced formula
param :: String -> String -> SemM 'Def Parameter
param name ty = do
  let p = Parameter { pName = name
                    , pType = ty
                    }
  RWS.modify' $ \f -> f { fOperands = fOperands f Seq.|> p }
  return p

-- | Mark a parameter as an input
input :: Parameter -> SemM 'Def ()
input p = RWS.modify' $ \f -> f { fInputs = ParamLoc p : fInputs f }

inputLiteral :: String -> SemM 'Def ()
inputLiteral l = RWS.modify' $ \f -> f { fInputs = LiteralLoc l : fInputs f }

-- | Define a location as an expression
defLoc :: Location -> Expr tp -> SemM 'Def ()
defLoc loc e = RWS.modify' $ \f -> f { fDefs = (loc, Some e) : fDefs f }

uf :: ExprType -> String -> [Some Expr] -> Expr tp
uf = UninterpretedFunc

bvadd :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
bvadd = binBVBuiltin "bvadd"

bvsub :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
bvsub = binBVBuiltin "bvsub"

bvmul :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
bvmul = binBVBuiltin "bvmul"

bvxor :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
bvxor = binBVBuiltin "bvxor"

bvor :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
bvor = binBVBuiltin "bvor"

bvand :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
bvand = binBVBuiltin "bvand"

bvshl :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
bvshl = binBVBuiltin "bvshl"

bvlshr :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
bvlshr = binBVBuiltin "bvlshr"

bvult :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBool
bvult = binTestBuiltin "bvult"

bvule :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBool
bvule = binTestBuiltin "bvule"

bvugt :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBool
bvugt = binTestBuiltin "bvugt"

bvuge :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBool
bvuge = binTestBuiltin "bvuge"

bvslt :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBool
bvslt = binTestBuiltin "bvslt"

bvsle :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBool
bvsle = binTestBuiltin "bvsle"

bvsgt :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBool
bvsgt = binTestBuiltin "bvsgt"

bvsge :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBool
bvsge = binTestBuiltin "bvsge"

ite :: (HasCallStack) => Expr 'TBool -> Expr tp -> Expr tp -> Expr tp
ite b t e
  | t1 == t2 && tc == EBool = Builtin t1 "ite" [Some b, Some t, Some e]
  | otherwise = error (printf "Unexpected type for ite: %s (should be TBool); %s and %s (should be equal)" (show t1) (show t2) (show tc))
  where
    t1 = exprType t
    t2 = exprType e
    tc = exprType b

-- | Bitwise not (complement)
bvnot :: (HasCallStack) => Expr 'TBV -> Expr 'TBV
bvnot e = Builtin (exprType e) "bvnot" [Some e]

binBVBuiltin :: (HasCallStack) => String -> Expr tp1 -> Expr tp1 -> Expr tp2
binBVBuiltin s e1 e2
  | t1 == t2 = Builtin t1 s [Some e1, Some e2]
  | otherwise = error (printf "Type mismatch for bitvector builtin; lhs type is %s while rhs type is %s" (show t1) (show t2))
  where
    t1 = exprType e1
    t2 = exprType e2

binTestBuiltin :: (HasCallStack) => String -> Expr 'TBV -> Expr 'TBV -> Expr 'TBool
binTestBuiltin s e1 e2
  | t1 == t2 = Builtin EBool s [Some e1, Some e2]
  | otherwise = error (printf "Type mismatch for bitvector test builtin; lhs type is %s while rhs type is %s" (show t1) (show t2))
  where
    t1 = exprType e1
    t2 = exprType e2

-- | The extract operation defined on bitvectors in SMTLib
--
-- Checks to ensure that the requested bits are in bounds and marks the size of
-- the new bitvector.
extract :: (HasCallStack)
        => Int
        -- ^ i
        -> Int
        -- ^ j
        -> Expr 'TBV
        -- ^ A bitvector expression
        -> Expr 'TBV
extract i j e =
  case exprType e of
    EBV w ->
      let newWidth = i - j + 1
      in case i < w && j <= i && j >= 0 && i >= 0 of
        True -> TheoryFunc (EBV newWidth) "extract" [Some (LitInt (fromIntegral i)), Some (LitInt (fromIntegral j))] [Some e]
        False -> error (printf "Invalid slice (%d,%d) of a %d-bit vector" i j w)
    ty -> error (printf "Invalid bitvector type %s" (show ty))

-- | Zero extend a value (add the requested number of zeros on the left)
--
-- The new type of the expression reflects the increased bit width
zeroExtend :: (HasCallStack)
           => Int
           -- ^ The number of bits to extend by
           -> Expr 'TBV
           -- ^ The expression to extend
           -> Expr 'TBV
zeroExtend n e =
  case exprType e of
    EBV w -> TheoryFunc (EBV (w + n)) "zero_extend" [Some (LitInt (fromIntegral n))] [Some e]
    ty -> error (printf "Unexpected non-bitvector type: %s" (show ty))

signExtend :: (HasCallStack)
           => Int
           -- ^ The number of bits to extend by
           -> Expr 'TBV
           -- ^ The expression to extend
           -> Expr 'TBV
signExtend n e =
  case exprType e of
    EBV w -> TheoryFunc (EBV (w + n)) "sign_extend" [Some (LitInt (fromIntegral n))] [Some e]
    ty -> error (printf "Unexpected non-bitvector type: %s" (show ty))

-- | Concatenate two bitvectors
concat :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
concat e1 e2 =
  case (exprType e1, exprType e2) of
    (EBV w1, EBV w2) -> Builtin (EBV (w1 + w2)) "concat" [Some e1, Some e2]
    (t1, t2) -> error (printf "Unexpected types: %s <> %s" (show t1) (show t2))


-- SExpression conversion

mkSExprs :: Seq.Seq Formula -> [(String, Definition)]
mkSExprs = map toSExpr . F.toList

toSExpr :: Formula -> (String, Definition)
toSExpr f = (fName f, Definition (fComment f) (extractSExpr (F.toList (fOperands f)) (fInputs f) (fDefs f)))

extractSExpr :: [Parameter] -> [Location] -> [(Location, Some Expr)] -> SC.SExpr FAtom
extractSExpr operands inputs defs =
  fromFoldable' [ SC.SCons (SC.SAtom (AIdent "operands")) (SC.SCons (convertOperands operands) SC.SNil)
                , SC.SCons (SC.SAtom (AIdent "in")) (SC.SCons (convertInputs inputs) SC.SNil)
                , SC.SCons (SC.SAtom (AIdent "defs")) (SC.SCons (convertDefs defs) SC.SNil)
                ]

convertExpr :: Some Expr -> SC.SExpr FAtom
convertExpr (Some e) =
  case e of
    LitInt i -> int i
    LitBV w val -> SC.SAtom (ABV w val)
    Loc _ l -> quoted l
    Param _ p -> ident (pName p)
    Builtin _ name params ->
      fromFoldable' (ident name : map convertExpr params)
    TheoryFunc _ name conParams appParams ->
      fromFoldable' (fromFoldable' (ident "_" : ident name : map convertExpr conParams) : map convertExpr appParams)
    UninterpretedFunc _ name params ->
      fromFoldable' (fromFoldable' [ident "_", ident "call", string name] : map convertExpr params)

convertDefs :: [(Location, Some Expr)] -> SC.SExpr FAtom
convertDefs = fromFoldable' . map convertDef
  where
    convertDef (loc, e) =
      case loc of
        ParamLoc p -> SC.SCons (ident (pName p)) (SC.SCons (convertExpr e) SC.SNil)
        LiteralLoc l -> SC.SCons (quoted l) (SC.SCons (convertExpr e) SC.SNil)

convertOperands :: [Parameter] -> SC.SExpr FAtom
convertOperands = fromFoldable' . map paramToDecl
  where
    paramToDecl p = SC.SCons (ident (pName p)) (quoted (pType p))

convertInputs :: [Location] -> SC.SExpr FAtom
convertInputs = fromFoldable' . map locToExpr
  where
    locToExpr l =
      case l of
        ParamLoc p -> ident (pName p)
        LiteralLoc s -> quoted s

-- | Turn any 'Foldable' into an s-expression by transforming each element with
-- the given function, then assembling as you would expect.
fromFoldable :: (F.Foldable f) => (a -> SC.SExpr atom) -> f a -> SC.SExpr atom
fromFoldable f = F.foldr (SC.SCons . f) SC.SNil

-- | @fromFoldable id@
fromFoldable' :: (F.Foldable f) => f (SC.SExpr atom) -> SC.SExpr atom
fromFoldable' = fromFoldable id

string :: String -> SC.SExpr FAtom
string = SC.SAtom . AString

-- | Lift an unquoted identifier.
ident :: String -> SC.SExpr FAtom
ident = SC.SAtom . AIdent

-- | Lift a quoted identifier.
quoted :: String -> SC.SExpr FAtom
quoted = SC.SAtom . AQuoted

-- | Lift an integer.
int :: Integer -> SC.SExpr FAtom
int = SC.SAtom . AInt

data FAtom = AIdent String
           | AQuoted String
           | AString String
           | AInt Integer
           | ABV Int Integer
           deriving (Show)

printDefinition :: Definition -> T.Text
printDefinition (Definition mc sexpr) =
  formatComment mc <> SC.encodeOne (SC.basicPrint printAtom) sexpr

formatComment :: Seq.Seq String -> T.Text
formatComment c
  | Seq.null c = T.empty
  | otherwise = T.pack $ unlines $ fmap formatLine (F.toList c)
  where
    formatLine l = printf ";; %s\n" l

printAtom :: FAtom -> T.Text
printAtom a =
  case a of
    AIdent s -> T.pack s
    AQuoted s -> T.pack ('\'' : s)
    AString s -> T.pack (show s)
    AInt i -> T.pack (show i)
    ABV w val -> formatBV w val

formatBV :: Int -> Integer -> T.Text
formatBV w val = T.pack (prefix ++ printf fmt val)
  where
    (prefix, fmt)
      | w `rem` 4 == 0 = ("#x", "%0" ++ show (w `div` 4) ++ "x")
      | otherwise = ("#b", "%0" ++ show w ++ "b")
