{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | A DSL to help defining instruction semantics to populate the base set (and manual set)

module SemMC.DSL (
  -- * Definitions
  defineOpcode,
  forkDefinition,
  param,
  input,
  defLoc,
  comment,
  -- * Architecture-specific Data support
  getArchData,
  setArchData,
  modifyArchData,
  -- * Operations
  (=:),
  testBitDynamic,
  extract,
  zeroExtend,
  signExtend,
  concat,
  ite,
  cases,
  uf,
  locUF,
  unpackUF,
  unpackLocUF,
  -- * Logical operations
  andp,
  orp,
  xorp,
  notp,
  -- ** Arithmetic bitvector ops
  bvadd,
  bvsub,
  bvmul,
  bvsdiv,
  bvudiv,
  bvurem,
  -- ** Bitwise bitvector ops
  bvxor,
  bvor,
  bvand,
  bvshl,
  bvashr,
  bvlshr,
  bvnot,
  bvclz,
  bvpopcnt,
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
  -- * Special values
  undefinedBV,
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
  SemMD,
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
import qualified Data.SCargot.Repr as SC
import           Data.Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Text.Printf ( printf )

import           Data.Parameterized.Some ( Some(..) )

import           SemMC.DSL.Internal
import           SemMC.Formula.SETokens ( FAtom(..), fromFoldable', printTokens
                                        , ident, int, quoted, string )


locationType :: Location tp -> ExprType tp
locationType loc =
  case loc of
    ParamLoc p -> pExprType p
    LiteralLoc ll -> lExprType ll
    LocationFunc t _ _ -> t

exprType :: Expr tp -> ExprType tp
exprType e =
  case e of
    LitBool _ -> EBool
    LitBV w _ -> EBV w
    LitInt _ -> EInt
    LitString _ -> EString
    Loc ll -> locationType ll
    Builtin t _ _ -> t
    TheoryFunc t _ _ _ -> t
    UninterpretedFunc t _ _ -> t
    NamedSubExpr _ sube -> exprType sube
    PackedOperand s -> EPackedOperand s

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
    NamedSubExpr _ sube -> exprBVSize sube


-- | The definition of the Formula that semantically describes the
-- functionality of an architecture's opcode.
--
-- Each opcode has a name (which is used to match the Dismantle
-- Instruction), a declaration of the operands (which should match the
-- instruction in type and ordering), the set inputs to consider for
-- this formula, and the actual definitions set by this formula
-- (i.e. changes to memory, register value, processor state, etc.)
-- There can also be comments for this formula (suggestions are
-- document references and further identifying information beyond just
-- the name of the opcode).
--
-- The SemM monad below defines a context in which a DSL for creating
-- the formula can be expressed.  The Formula is then written to a
-- descriptive semantics file as an S-expression; this S-expression
-- can be read in later to perform evaluation of the formula to
-- compute results (often symbolically).
--
-- The 'd' type argument is an extension type for additional data
-- maintained by the architecture-specific implementation of this
-- monad DSL.  This specifies the type of additional data that can be
-- kept in the state and used for processing; it is ignored for the
-- generation of the final S-Expression.
data Formula d = Formula { fName :: String
                         , fOperands :: Seq.Seq (Some Parameter)
                         , fInputs :: [Some Location]
                         , fDefs :: [(Some Location, Some Expr)]
                         , fComment :: Seq.Seq String
                         -- ^ Comments stored as individual lines
                         , fArchData :: Maybe d
                         }
    -- n.b. it could be convenient to automatically derive a Show
    -- instance for Formula, but this would require a Show d
    -- constraint here and in all the DSL function declarations, which
    -- would be annoying.  Feel free to create an explicit show
    -- instance that shows everything but the archData.


-- | Generate a new, blank formula with the specified name (opcode)
newFormula :: String -> Formula d
newFormula name = Formula { fName = name
                          , fComment = Seq.empty
                          , fOperands = Seq.empty
                          , fInputs = []
                          , fDefs = []
                          , fArchData = Nothing
                          }

-- | The state component of the monad is a Formula that is built up during a
-- single definition; after the definition, it is added to the output sequence.
--
-- It is actually possible to generate several formulas during the
-- execution of this monad (see 'forkDefinition' below).  The writer
-- portion is used to store these completed formulas.
--
-- The @t@ is a phantom parameter to ensure that nesting definitions is
-- impossible.
--
newtype SemMD (t :: Phase) d a =
    SemM { unSem :: RWS.RWS () (Seq.Seq (Formula d)) (Formula d) a }
          deriving (Functor,
                    Applicative,
                    Monad,
                    RWS.MonadWriter (Seq.Seq (Formula d)),
                    RWS.MonadState (Formula d))


-- | Simpler form of 'SemMD' for for architectures that do not need
-- any architectore-specific data maintained.
type SemM (t :: Phase) a = SemMD t () a

-- | Tags used as phantom types to prevent nested opcode definitions
data Phase = Top | Def

data Definition = Definition (Seq.Seq String) (SC.SExpr FAtom)
  deriving (Show)

-- | Run a semantics defining action and return the defined formulas.
--
-- The result is an association list from opcode name to the s-expression
-- representing it.
runSem :: SemMD 'Top d () -> [(String, Definition)]
runSem act = mkSExprs (snd (RWS.execRWS (unSem act) () (newFormula "")))
    -- The initial dummy formula here is never used.  It is just a standin until
    -- the first call to 'defineOpcode'.  If 'defineOpcode' is never called,
    -- this will never be used since 'defineOpcode' handles adding the result to
    -- the writer output.


-- | Define an opcode with a given name.
--
-- The body is executed to produce a definition.
defineOpcode :: String -> SemMD 'Def d () -> SemMD 'Top d ()
defineOpcode name (SemM def) = do
  RWS.put $ newFormula name
  SemM def
  formula <- RWS.get
  RWS.tell (Seq.singleton formula)
  return ()

-- | Fork a definition into a second definition under a different name
--
-- This is designed to allow defining an instruction that is a strict extension
-- of another instruction.  Note that comments are not preserved, and the new
-- definition is given a new name.
--
-- > defineOpcode "OP1" $ do
-- >   comment ...
-- >   x <- param ...
-- >   defLoc x ...
-- >
-- >   forkDefinition "OP1'" $ do
-- >     comment ...
-- >     defLoc eflags ...
forkDefinition :: String -> SemMD 'Def d () -> SemMD 'Def d ()
forkDefinition name (SemM def) = do
  origFormula <- RWS.get
  let modFormula = origFormula { fName = name
                               , fComment = Seq.empty
                               }
  RWS.put modFormula
  SemM def
  forkedFormula <- RWS.get
  RWS.tell (Seq.singleton forkedFormula)
  -- Restore the original formula so that 'definOpcode' can finish it off
  RWS.put origFormula

-- | Add a descriptive comment to the output file
--
-- Each call appends a new comment line.  Individual calls to comment should not
-- contain newlines.
comment :: String -> SemMD 'Def d ()
comment c = RWS.modify' $ \f -> f { fComment = fComment f Seq.|> c }

-- | Declare a named parameter; the 'name' string provided is used as
-- the variable name in the produced formula, the 'ty' string
-- specifies the type (strings are types via TypeLits), and the 'ety'
-- specifies the expression type for this parameter.  The result is a
-- Location reference to that parameter.
param :: String -> String -> ExprType tp -> SemMD 'Def d (Location tp)
param name ty ety = do
  let p = Parameter { pName = name
                    , pType = ty
                    , pExprType = ety
                    }
  RWS.modify' $ \f -> f { fOperands = fOperands f Seq.|> Some p }
  return (ParamLoc p)

-- | Mark a parameter as an input
input :: Location tp -> SemMD 'Def d ()
input loc = RWS.modify' $ \f -> f { fInputs = Some loc : fInputs f }

-- | Define a location as an expression
defLoc :: (HasCallStack) => Location tp -> Expr tp -> SemMD 'Def d ()
defLoc loc e
  | locationType loc == exprType e = do
      curDefs <- RWS.gets fDefs
      case lookup (Some loc) curDefs of
        Nothing -> RWS.modify' $ \f -> f { fDefs = (Some loc, Some e) : fDefs f }
        Just _ -> error (printf "Location is already defined: %s" (show loc))
  | otherwise = error (printf "Type mismatch; got %s but expected %s" (show (exprType e)) (show (locationType loc)))


-- | Define a subphrase that is a natural candidate for a let binding
-- with the associated variable name.
infix 1 =:
(=:) :: String -> Expr tp -> Expr tp
name =: expr = NamedSubExpr name expr


-- | Get the current architecture-specific data in the DSL computation
getArchData :: SemMD t d (Maybe d)
getArchData = fArchData <$> RWS.get


-- | Set the current architecture-specific data in the DSL computation
setArchData :: Maybe d -> SemMD t d ()
setArchData m'ad = RWS.modify (\s -> s { fArchData = m'ad })


-- | Modify the current architecture-specific data in the DSL computation
modifyArchData :: (Maybe d -> Maybe d) -> SemMD t d ()
modifyArchData adf = RWS.modify (\s -> s { fArchData = adf (fArchData s) })


-- ----------------------------------------------------------------------
-- Expressions

-- | Allow for user-defined functions over expressions
uf :: ExprType tp -> String -> [Some Expr] -> Expr tp
uf = UninterpretedFunc

-- | Allow for user-defined functions over locations
locUF :: ExprType tp -> String -> Location tp' -> Location tp
locUF = LocationFunc

-- | Unpack a specific operand type using an architecture-specific
-- uninterpreted function
unpackUF :: String -> ExprType tp -> String -> Location 'TPackedOperand -> Expr tp
unpackUF operandName rtype ufname fromParam =
    case exprType (Loc fromParam) of
      EPackedOperand o | operandName == o -> uf rtype ufname [Some $ Loc fromParam]
                       | otherwise -> error $ ufname <> " expected a \"" <> operandName <> "\" but got a " <> o

-- | Unpack a specific operand type using an architecture-specific
-- undefined function
unpackLocUF :: String -> ExprType tp -> String -> Location 'TPackedOperand -> Location tp
unpackLocUF operandName rtype ufname fromParam =
    case exprType (Loc fromParam) of
      EPackedOperand o | operandName == o -> locUF rtype ufname fromParam
                       | otherwise -> error $ ufname <> " expected a \"" <> operandName <> "\" location but got a " <> o

-- | Create an expression of bitvector type that represents an undefined value
-- of the given size
undefinedBV :: (HasCallStack) => Int -> Expr 'TBV
undefinedBV size = uf (EBV size) "undefined" [ Some (LitBV 32 (toInteger size)) ]

bvadd :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
bvadd = binBVBuiltin "bvadd"

bvsub :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
bvsub = binBVBuiltin "bvsub"

bvmul :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
bvmul = binBVBuiltin "bvmul"

bvudiv :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
bvudiv = binBVBuiltin "bvudiv"

bvurem :: Expr 'TBV -> Expr 'TBV -> Expr 'TBV
bvurem = binBVBuiltin "bvurem"

bvsdiv :: ({- HasCallStack -}) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
bvsdiv = binBVBuiltin "bvsdiv"

bvxor :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
bvxor = binBVBuiltin "bvxor"

bvor :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
bvor = binBVBuiltin "bvor"

bvand :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
bvand = binBVBuiltin "bvand"

bvshl :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
bvshl bv n
  | LitBV _ 0 <- n = bv
  | otherwise = binBVBuiltin "bvshl" bv n

bvlshr :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
bvlshr bv n
  | LitBV _ 0 <- n = bv
  | otherwise = binBVBuiltin "bvlshr" bv n

bvashr :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
bvashr bv n
  | LitBV _ 0 <- n = bv
  | otherwise = binBVBuiltin "bvashr" bv n

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
bveq x y = maybe (binTestBuiltin "bveq" x y) id $ litEq x y

bvne :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBool
bvne x y = maybe (binTestBuiltin "bvne" x y) notp $ litEq x y

notp :: (HasCallStack) => Expr 'TBool -> Expr 'TBool
notp e =
  case e of
    LitBool True -> LitBool False
    LitBool False -> LitBool True
    _ -> Builtin EBool "notp" [Some e]

andp :: (HasCallStack) => Expr 'TBool -> Expr 'TBool -> Expr 'TBool
andp e1 e2 =
  case (e1, e2) of
    (LitBool True, _) -> e2
    (_, LitBool True) -> e1
    (LitBool False, _) -> LitBool False
    (_, LitBool False) -> LitBool False
    _ -> boolBinopBuiltin "andp" e1 e2

orp :: (HasCallStack) => Expr 'TBool -> Expr 'TBool -> Expr 'TBool
orp e1 e2 =
  case (e1, e2) of
    (LitBool True, _) -> LitBool True
    (_, LitBool True) -> LitBool True
    (LitBool False, _) -> e2
    (_, LitBool False) -> e1
    _ -> boolBinopBuiltin "orp" e1 e2

xorp :: (HasCallStack) => Expr 'TBool -> Expr 'TBool -> Expr 'TBool
xorp = boolBinopBuiltin "xorp"

boolBinopBuiltin :: (HasCallStack) => String -> Expr 'TBool -> Expr 'TBool -> Expr 'TBool
boolBinopBuiltin s e1 e2 = Builtin EBool  s [Some e1, Some e2]

ite :: (HasCallStack) => Expr 'TBool -> Expr tp -> Expr tp -> Expr tp
ite b t e =
  if comparable t == comparable e
  then t
  else case b of
         LitBool True -> t
         LitBool False -> e
         _ | t1 == t2 && tc == EBool -> Builtin t1 "ite" [Some b, Some t, Some e]
           | otherwise -> error (printf "Unexpected type for ite: %s\
                                        \ (should be TBool); %s and %s\
                                        \ (should be equal)"
                                 (show tc) (show t1) (show t2))
  where
    t1 = exprType t
    t2 = exprType e
    tc = exprType b
    comparable = convertExpr . Some

-- | Construct a nested chain of ITEs that is equivalent to a case match with a
-- fallthrough.
cases :: (HasCallStack) => [(Expr 'TBool, Expr a)] -> Expr a -> Expr a
cases cs els =
  case cs of
    [] -> els
    (c, thn) : rest -> ite c thn (cases rest els)

-- | Bitwise not (complement)
bvnot :: (HasCallStack) => Expr 'TBV -> Expr 'TBV
bvnot e = Builtin (exprType e) "bvnot" [Some e]

-- | Count leading zeros
bvclz :: (HasCallStack) => Expr 'TBV -> Expr 'TBV
bvclz e =
  case exprType e of
    EBV n -> uf (exprType e) (printf "clz.%d" n) [ Some e ]

-- | Population count (count number of bits set)
bvpopcnt :: (HasCallStack) => Expr 'TBV -> Expr 'TBV
bvpopcnt e =
  case exprType e of
    EBV n -> uf (exprType e) (printf "popcnt.%d" n) [ Some e ]

binBVBuiltin :: (HasCallStack) => String -> Expr tp1 -> Expr tp1 -> Expr tp1
binBVBuiltin s e1 e2
  | t1 == t2 = Builtin t1 s [Some e1, Some e2]
  | otherwise = error (printf "Type mismatch for bitvector builtin %s; lhs type is %s while rhs type is %s" s (show t1) (show t2))
  where
    t1 = exprType e1
    t2 = exprType e2

binTestBuiltin :: (HasCallStack) => String -> Expr 'TBV -> Expr 'TBV -> Expr 'TBool
binTestBuiltin s e1 e2
  | t1 == t2 = Builtin EBool s [Some e1, Some e2]
  | otherwise = error (printf "Type mismatch for bitvector test builtin %s; lhs type is %s while rhs type is %s" s (show t1) (show t2))
  where
    t1 = exprType e1
    t2 = exprType e2

-- | Test a dynamically-chosen bit number (i.e., the bit number to test is an
-- expr and not an 'Int')
testBitDynamic :: (HasCallStack)
               => Expr 'TBV -- ^ Bit number to test
               -> Expr 'TBV
               -> Expr 'TBool
testBitDynamic bitNum e = uf EBool "test_bit_dynamic" [Some bitNum, Some e]

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
        -- ^ i (the highest bit number in the range to extract, inclusive)
        -> Int
        -- ^ j (the lowest bit number in the range to extract, inclusive)
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


-- ----------------------------------------------------------------------
-- SExpression conversion

mkSExprs :: Seq.Seq (Formula d) -> [(String, Definition)]
mkSExprs = map toSExpr . F.toList

toSExpr :: (Formula d) -> (String, Definition)
toSExpr f = (fName f, Definition (fComment f) (extractSExpr (F.toList (fOperands f)) (fInputs f) (fDefs f)))

extractSExpr :: [Some Parameter] -> [Some Location] -> [(Some Location, Some Expr)] -> SC.SExpr FAtom
extractSExpr operands inputs defs =
  fromFoldable' [ SC.SCons (SC.SAtom (AIdent "operands")) (SC.SCons (convertOperands operands) SC.SNil)
                , SC.SCons (SC.SAtom (AIdent "in")) (SC.SCons (convertInputs inputs) SC.SNil)
                , SC.SCons (SC.SAtom (AIdent "defs")) (SC.SCons (convertDefs defs) SC.SNil)
                ]

convertExpr :: Some Expr -> SC.SExpr FAtom
convertExpr (Some e) =
  let samevals = [Some $ LitBV 1 0, Some $ LitBV 1 0] in
  case e of
    -- there is no atomic True or False value, so represent those as
    -- an expression, but use the base expression form without any
    -- possible re-evaluation to avoid recursion.
    LitBool True -> convertExpr $ Some $ NamedSubExpr "true" $ Builtin EBool "bveq" samevals
    LitBool False -> convertExpr $ Some $ NamedSubExpr "false" $ Builtin EBool "bvne" samevals
    LitInt i -> int i
    LitString s -> string s
    LitBV w val -> SC.SAtom (ABV w val)
    Loc loc -> convertLoc loc
    Builtin _ name params ->
      fromFoldable' (ident name : map convertExpr params)
    TheoryFunc _ name conParams appParams ->
      fromFoldable' (fromFoldable' (ident "_" : ident name : map convertExpr conParams) : map convertExpr appParams)
    UninterpretedFunc _ name params ->
      fromFoldable' (fromFoldable' [ident "_", ident "call", string name] : map convertExpr params)
    NamedSubExpr name expr ->
        let tag d = \case
                    SC.SNil -> SC.SNil  -- no tagging of nil elements
                    SC.SAtom a -> SC.SAtom $ ANamed name d a
                    SC.SCons l r -> SC.SCons (tag (d+1) l) r
        in tag 0 $ convertExpr $ Some expr
    PackedOperand name -> error "PackedOperand not unpacked with unpackUF.. cannot serialize"

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


printDefinition :: Definition -> T.Text
printDefinition (Definition mc sexpr) = printTokens mc sexpr
