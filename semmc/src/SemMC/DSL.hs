{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
-- | A DSL to help defining instruction semantics to populate the base set (and manual set)
module SemMC.DSL (
  -- * Definitions
  defineOpcode,
  param,
  input,
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
  Location(..),
  -- * Monad
  SemM,
  Phase(..),
  runSem,
  Parameter,
  Definition,
  printDefinition
  ) where

import           Prelude hiding ( concat )

import qualified Control.Monad.RWS.Strict as RWS
import qualified Data.Foldable as F
import           Data.Monoid
import qualified Data.SCargot as SC
import qualified Data.SCargot.Repr as SC
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Text.Printf ( printf )

-- | An expression representing an SMT formula.  It can reference parameters
data Expr = LitBV Int Integer
          | LitInt Integer
          | Loc String
          | Param Parameter
          | Builtin String [Expr]
          -- ^ Built-in operations
          | TheoryFunc String [Expr] [Expr]
          -- ^ Functions provided by SMT theories like extract and zero_extend;
          -- the first list is the list of operands passed to the function
          -- constructor, while the second list is the list of arguments that
          -- the resulting function is applied to.
          | UninterpretedFunc String [Expr]
          -- ^ A call to an uninterpreted function, applied to the given arguments
          deriving (Show)

-- | A parameter and its type
--
-- The type is a string corresponding to an operand type from the architecture
-- (e.g., Gprc)
data Parameter = Parameter { pName :: String
                           , pType :: String
                           }
               deriving (Show)

data Location = ParamLoc Parameter
              | LiteralLoc String
              deriving (Show)

data Formula = Formula { fName :: String
                       , fOperands :: Seq.Seq Parameter
                       , fInputs :: [Parameter]
                       , fDefs :: [(Location, Expr)]
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
param :: String -> String -> SemM 'Def Parameter
param name ty = do
  let p = Parameter { pName = name
                    , pType = ty
                    }
  RWS.modify' $ \f -> f { fOperands = fOperands f Seq.|> p }
  return p

-- | Mark a parameter as an input
input :: Parameter -> SemM 'Def ()
input p = RWS.modify' $ \f -> f { fInputs = p : fInputs f }

-- | Define a location as an expression
defLoc :: Location -> Expr -> SemM 'Def ()
defLoc loc e = RWS.modify' $ \f -> f { fDefs = (loc, e) : fDefs f }

uf :: String -> [Expr] -> Expr
uf = UninterpretedFunc

bvadd :: Expr -> Expr -> Expr
bvadd = binBuiltin "bvadd"

bvsub :: Expr -> Expr -> Expr
bvsub = binBuiltin "bvsub"

bvmul :: Expr -> Expr -> Expr
bvmul = binBuiltin "bvmul"

bvxor :: Expr -> Expr -> Expr
bvxor = binBuiltin "bvxor"

bvor :: Expr -> Expr -> Expr
bvor = binBuiltin "bvor"

bvand :: Expr -> Expr -> Expr
bvand = binBuiltin "bvand"

bvshl :: Expr -> Expr -> Expr
bvshl = binBuiltin "bvshl"

bvlshr :: Expr -> Expr -> Expr
bvlshr = binBuiltin "bvlshr"

bvult :: Expr -> Expr -> Expr
bvult = binBuiltin "bvult"

bvule :: Expr -> Expr -> Expr
bvule = binBuiltin "bvule"

bvugt :: Expr -> Expr -> Expr
bvugt = binBuiltin "bvugt"

bvuge :: Expr -> Expr -> Expr
bvuge = binBuiltin "bvuge"

bvslt :: Expr -> Expr -> Expr
bvslt = binBuiltin "bvslt"

bvsle :: Expr -> Expr -> Expr
bvsle = binBuiltin "bvsle"

bvsgt :: Expr -> Expr -> Expr
bvsgt = binBuiltin "bvsgt"

bvsge :: Expr -> Expr -> Expr
bvsge = binBuiltin "bvsge"

ite :: Expr -> Expr -> Expr -> Expr
ite b t e = Builtin "ite" [b, t, e]

-- | Bitwise not (complement)
bvnot :: Expr -> Expr
bvnot e = Builtin "bvnot" [e]

binBuiltin :: String -> Expr -> Expr -> Expr
binBuiltin s e1 e2 = Builtin s [e1, e2]

-- | The extract operation defined on bitvectors in SMTLib
extract :: Int
        -- ^ i
        -> Int
        -- ^ j
        -> Expr
        -- ^ A bitvector expression
        -> Expr
extract i j e = TheoryFunc "extract" [LitInt (fromIntegral i), LitInt (fromIntegral j)] [e]

zeroExtend :: Int
           -- ^ The number of bits to extend by
           -> Expr
           -- ^ The expression to extend
           -> Expr
zeroExtend n e = TheoryFunc "zero_extend" [LitInt (fromIntegral n)] [e]

signExtend :: Int
           -- ^ The number of bits to extend by
           -> Expr
           -- ^ The expression to extend
           -> Expr
signExtend n e = TheoryFunc "sign_extend" [LitInt (fromIntegral n)] [e]

-- | Concatenate two bitvectors
concat :: Expr -> Expr -> Expr
concat e1 e2 = Builtin "concat" [e1, e2]


-- SExpression conversion

mkSExprs :: Seq.Seq Formula -> [(String, Definition)]
mkSExprs = map toSExpr . F.toList

toSExpr :: Formula -> (String, Definition)
toSExpr f = (fName f, Definition (fComment f) (extractSExpr (F.toList (fOperands f)) (fInputs f) (fDefs f)))

extractSExpr :: [Parameter] -> [Parameter] -> [(Location, Expr)] -> SC.SExpr FAtom
extractSExpr operands inputs defs =
  fromFoldable' [ SC.SCons (SC.SAtom (AIdent "operands")) (SC.SCons (convertOperands operands) SC.SNil)
                , SC.SCons (SC.SAtom (AIdent "in")) (SC.SCons (convertInputs inputs) SC.SNil)
                , SC.SCons (SC.SAtom (AIdent "defs")) (SC.SCons (convertDefs defs) SC.SNil)
                ]

convertExpr :: Expr -> SC.SExpr FAtom
convertExpr e =
  case e of
    LitInt i -> int i
    LitBV w val -> SC.SAtom (ABV w val)
    Loc l -> quoted l
    Param p -> ident (pName p)
    Builtin name params ->
      fromFoldable' (ident name : map convertExpr params)
    TheoryFunc name conParams appParams ->
      fromFoldable' (fromFoldable' (ident "_" : ident name : map convertExpr conParams) : map convertExpr appParams)
    UninterpretedFunc name params ->
      fromFoldable' (fromFoldable' [ident "_", ident "call", string name] : map convertExpr params)

convertDefs :: [(Location, Expr)] -> SC.SExpr FAtom
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

convertInputs :: [Parameter] -> SC.SExpr FAtom
convertInputs = fromFoldable' . map (ident . pName)

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
