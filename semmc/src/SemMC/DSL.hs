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
  defLoc,
  comment,
  -- * Operations
  extractDynamic,
  extract,
  zeroExtend,
  signExtend,
  concat,
  ite,
  uf,
  locUF,
  -- * Logical operations
  andp,
  orp,
  xorp,
  notp,
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
  bveq,
  bvne,
  -- * Expressions
  Expr(..),
  ExprTag(..),
  ExprType(..),
  exprType,
  exprBVSize,
  Location(..),
  Literal(..),
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

import           Data.Parameterized.Some ( Some(..) )

import           SemMC.DSL.Internal





locationType :: Location tp -> ExprType tp
locationType loc =
  case loc of
    ParamLoc p -> pExprType p
    LiteralLoc ll -> lExprType ll
    LocationFunc t _ _ -> t

exprType :: Expr tp -> ExprType tp
exprType e =
  case e of
    LitBV w _ -> EBV w
    LitInt _ -> EInt
    Loc ll -> locationType ll
    Builtin t _ _ -> t
    TheoryFunc t _ _ _ -> t
    UninterpretedFunc t _ _ -> t

-- | Get the size of the bitvector produced by the given expression
exprBVSize :: Expr 'TBV -> Int
exprBVSize e =
  case e of
    LitBV w _ -> w
    Loc ll ->
      case locationType ll of
        EBV w -> w
    Builtin (EBV w) _ _ -> w
    TheoryFunc (EBV w) _ _ _ -> w
    UninterpretedFunc (EBV w) _ _ -> w


data Formula = Formula { fName :: String
                       , fOperands :: Seq.Seq (Some Parameter)
                       , fInputs :: [Some Location]
                       , fDefs :: [(Some Location, Some Expr)]
                       , fComment :: Seq.Seq String
                       -- ^ Comments stored as individual lines
                       }
             deriving (Show)

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
param :: String -> String -> ExprType tp -> SemM 'Def (Location tp)
param name ty ety = do
  let p = Parameter { pName = name
                    , pType = ty
                    , pExprType = ety
                    }
  RWS.modify' $ \f -> f { fOperands = fOperands f Seq.|> Some p }
  return (ParamLoc p)

-- | Mark a parameter as an input
input :: Location tp -> SemM 'Def ()
input loc = RWS.modify' $ \f -> f { fInputs = Some loc : fInputs f }

-- | Define a location as an expression
defLoc :: (HasCallStack) => Location tp -> Expr tp -> SemM 'Def ()
defLoc loc e
  | locationType loc == exprType e = do
      curDefs <- RWS.gets fDefs
      case lookup (Some loc) curDefs of
        Nothing -> RWS.modify' $ \f -> f { fDefs = (Some loc, Some e) : fDefs f }
        Just _ -> error (printf "Location is already defined: %s" (show loc))
  | otherwise = error (printf "Type mismatch; got %s but expected %s" (show (exprType e)) (show (locationType loc)))

-- | Allow for user-defined functions over expressions
uf :: ExprType tp -> String -> [Some Expr] -> Expr tp
uf = UninterpretedFunc

-- | Allow for user-defined functions over locations
locUF :: ExprType tp -> String -> Location tp' -> Location tp
locUF = LocationFunc

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

bveq :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBool
bveq = binTestBuiltin "bveq"

bvne :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBool
bvne = binTestBuiltin "bvne"

notp :: (HasCallStack) => Expr 'TBool -> Expr 'TBool
notp e = Builtin EBool "notp" [Some e]

andp :: (HasCallStack) => Expr 'TBool -> Expr 'TBool -> Expr 'TBool
andp = boolBinopBuiltin "andp"

orp :: (HasCallStack) => Expr 'TBool -> Expr 'TBool -> Expr 'TBool
orp = boolBinopBuiltin "orp"

xorp :: (HasCallStack) => Expr 'TBool -> Expr 'TBool -> Expr 'TBool
xorp = boolBinopBuiltin "xorp"

boolBinopBuiltin :: (HasCallStack) => String -> Expr 'TBool -> Expr 'TBool -> Expr 'TBool
boolBinopBuiltin s e1 e2 = Builtin EBool  s [Some e1, Some e2]

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

binBVBuiltin :: (HasCallStack) => String -> Expr tp1 -> Expr tp1 -> Expr tp1
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

-- | Like extract, but where the indexes to extract are only known dynamically
-- (i.e., at instruction instantiation time).  The other version, 'extract',
-- requires the indexes to be known statically.
--
-- This is meant to be instantiated as a normal 'extract' when the instruction
-- is instantiated.
extractDynamic :: (HasCallStack)
               => Int -- ^ Result size
               -> Expr 'TBV -- ^ i
               -> Expr 'TBV -- ^ j
               -> Expr 'TBV -- ^ A bitvector
               -> Expr 'TBV
extractDynamic rsize i j e =
  uf (EBV rsize) "extract_dynamic" [Some i, Some j, Some e]

-- | The extract operation defined on bitvectors in SMTLib
--
-- Checks to ensure that the requested bits are in bounds and marks the size of
-- the new bitvector.
--
-- The SMTLib operation is:
--
--
-- >      ((_ extract i j) (_ BitVec m) (_ BitVec n))
-- >    where
-- >    - i, j, m, n are numerals
-- >    - m > i ≥ j ≥ 0,
-- >    - n = i - j + 1
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
      in case w > i && i >= j && i >= 0 of
        True -> TheoryFunc (EBV newWidth) "extract" [Some (LitInt (fromIntegral i)), Some (LitInt (fromIntegral j))] [Some e]
        False -> error (printf "Invalid slice (%d,%d) of a %d-bit vector" i j w)

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

signExtend :: (HasCallStack)
           => Int
           -- ^ The number of bits to extend by
           -> Expr 'TBV
           -- ^ The expression to extend
           -> Expr 'TBV
signExtend n e =
  case exprType e of
    EBV w -> TheoryFunc (EBV (w + n)) "sign_extend" [Some (LitInt (fromIntegral n))] [Some e]

-- | Concatenate two bitvectors
concat :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
concat e1 e2 =
  case (exprType e1, exprType e2) of
    (EBV w1, EBV w2) -> Builtin (EBV (w1 + w2)) "concat" [Some e1, Some e2]

-- SExpression conversion

mkSExprs :: Seq.Seq Formula -> [(String, Definition)]
mkSExprs = map toSExpr . F.toList

toSExpr :: Formula -> (String, Definition)
toSExpr f = (fName f, Definition (fComment f) (extractSExpr (F.toList (fOperands f)) (fInputs f) (fDefs f)))

extractSExpr :: [Some Parameter] -> [Some Location] -> [(Some Location, Some Expr)] -> SC.SExpr FAtom
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
    Loc loc -> convertLoc loc
    Builtin _ name params ->
      fromFoldable' (ident name : map convertExpr params)
    TheoryFunc _ name conParams appParams ->
      fromFoldable' (fromFoldable' (ident "_" : ident name : map convertExpr conParams) : map convertExpr appParams)
    UninterpretedFunc _ name params ->
      fromFoldable' (fromFoldable' [ident "_", ident "call", string name] : map convertExpr params)

convertLoc :: Location tp -> SC.SExpr FAtom
convertLoc loc =
  case loc of
    ParamLoc p -> ident (pName p)
    LiteralLoc ll -> quoted (lName ll)
    LocationFunc _ func loc' ->
      fromFoldable' [fromFoldable' [ident "_", ident "call", string func], convertLoc loc']

convertDefs :: [(Some Location, Some Expr)] -> SC.SExpr FAtom
convertDefs = fromFoldable' . map convertDef
  where
    convertDef (Some loc, e) = SC.SCons (convertLoc loc) (SC.SCons (convertExpr e) SC.SNil)

convertOperands :: [Some Parameter] -> SC.SExpr FAtom
convertOperands = fromFoldable' . map paramToDecl
  where
    paramToDecl (Some p) = SC.SCons (ident (pName p)) (quoted (pType p))

convertInputs :: [Some Location] -> SC.SExpr FAtom
convertInputs = fromFoldable' . map locToExpr
  where
    locToExpr (Some l) = convertLoc l

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
    formatLine l = printf ";; %s" l

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
