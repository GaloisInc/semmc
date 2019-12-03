{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | A DSL to help defining instruction semantics to populate the base set (and manual set)

module SemMC.DSL (
  -- * Definitions
  defineOpcode,
  defineOpcodeWithParams,
  forkDefinition,
  param,
  input,
  defLoc,
  comment,
  -- * Architecture-specific Data support
  getArchData,
  setArchData,
  modifyArchData,
  -- * Library functions
  Argument(..),
  LibraryFunctionDef,
  defineLibraryFunction,
  wrapAsLibraryFunction,
  SL.List (..),
  -- * Operations
  (=:),
  testBit,
  testBitDynamic,
  testBitDynamic32,
  testBitDynamic64,
  extract,
  zeroExtend, zeroExtendTo, zext',
  signExtend, signExtendTo, sext',
  concat,
  ite,
  cases,
  uf,
  locUF,
  lf,
  unpackUF,
  unpackLocUF,
  -- * Logical operations
  andp,
  orp,
  xorp,
  notp,
  anyp,
  allp,
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
  bvset,
  bvclr,
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
  bvPredToBit,
  bvUpdate,
  bvUpdate',
  -- * Floating-point operations
  fsqrt,
  fsqrts,
  fadd,
  fadds,
  fsub,
  fsubs,
  fmul,
  fmuls,
  fdiv,
  fdivs,
  ffma,
  ffmas,
  fnegd,
  fnegs,
  fabsd,
  fabss,
  fltd,
  flts,
  feqd,
  feqs,
  fled,
  fles,
  fnand,
  fnans,
  frsp,
  fctid,
  fctidu,
  fctiw,
  fctiwu,
  fcfid,
  fcfids,
  fcfidu,
  fcfidus,
  frti,
  frtis,
  fpDoubleToSingle,
  fpSingleToDouble,
  fpBinaryToDouble,
  fpBinaryToSingle,
  fpDoubleToBinary,
  fpSingleToBinary,
  -- * Special values
  undefinedBV,
  -- * Expressions
  Expr(..),
  ExprTag(..),
  ExprTypeRepr(..),
  exprType,
  exprBVSize,
  Location(..),
  Literal(..),
  -- * Monad
  SemM,
  SemMD,
  Phase(..),
  runSem,
  evalSem,
  Parameter(..),
  Package(..),
  Definition,
  printDefinition,
  FunctionDefinition,
  printFunctionDefinition
  ) where

import           GHC.Stack ( HasCallStack )

import qualified Control.Monad.RWS.Strict as RWS
import qualified Data.Foldable as F
import qualified Data.Bits as B
import qualified Data.Map as M
import qualified Data.SCargot.Repr as SC
import           Data.Semigroup hiding ( Arg )
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Text.Printf ( printf )

import qualified Data.Parameterized.List as SL
import qualified Data.Parameterized.NatRepr as NR
import           Data.Parameterized.Some ( Some(..), viewSome )
import           Data.Parameterized.TraversableFC ( fmapFC, foldMapFC
                                                  , traverseFC, toListFC )
import qualified Data.Type.List as DL

import           SemMC.DSL.Internal
import           SemMC.Formula.SETokens ( FAtom(..), fromFoldable', printTokens
                                        , ident, int, quoted, string )
import qualified What4.BaseTypes as CRU

import           Prelude hiding ( concat )


locationType :: Location tp -> ExprTypeRepr tp
locationType loc =
  case loc of
    ParamLoc p -> pExprTypeRepr p
    LiteralLoc ll -> lExprType ll
    MemoryLoc _ -> EMemory
    LocationFunc t _ _ -> t

exprType :: Expr tp -> ExprTypeRepr tp
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
    LibraryFunc (LibFuncDef { lfdRetType = t }) _ -> t
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
    LibraryFunc (LibFuncDef { lfdRetType = EBV w })  _ -> w
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
    SemM { unSem :: RWS.RWS () (Seq.Seq (Formula d)) (SemMDState d t) a }
          deriving (Functor,
                    Applicative,
                    Monad,
                    RWS.MonadWriter (Seq.Seq (Formula d)),
                    RWS.MonadState (SemMDState d t))

data SemMDState d t where
  DefState :: { smdFormula :: Formula d } -> SemMDState d 'Def
  TopState :: SemMDState d 'Top

-- | Simpler form of 'SemMD' for for architectures that do not need
-- any architectore-specific data maintained.
type SemM (t :: Phase) a = SemMD t () a

-- | Tags used as phantom types to prevent nested opcode definitions
data Phase = Top | Def

data Definition = Definition (Seq.Seq String) (SC.SExpr FAtom)
  deriving (Show)

newtype FunctionDefinition = FunctionDefinition (SC.SExpr FAtom)
  deriving (Show)

data Package = Package { pkgFormulas :: [(String, Definition)]
                       , pkgFunctions :: [(String, FunctionDefinition)]
                       }

-- | Run a semantics-defining action and return the defined formulas and any
-- library functions required.
--
-- The result is an association list from opcode name to the s-expression
-- representing it, plus an association list from function name to its
-- s-expression.
runSem :: SemMD 'Top d () -> Package
runSem act = snd $ evalSem act

-- | Run a semantics-defining action and return the defined formulas and library
-- functions, along with anything returned by the action itself.
evalSem :: SemMD 'Top d a -> (a, Package)
evalSem act = (a, Package defs (M.toList functions))
  where
    (a, ~TopState, formulas) = RWS.runRWS (unSem act) () TopState
    (defs, functions) = mkSExprs formulas

-- | Define an opcode with a given name.
--
-- The body is executed to produce a definition.
defineOpcode :: String -> SemMD 'Def d () -> SemMD 'Top d ()
defineOpcode name (SemM def) = do
  let state = DefState { smdFormula = newFormula name }
      !(~(), state', formulas) = RWS.runRWS def () state
  RWS.tell (formulas Seq.|> smdFormula state')

defineOpcodeWithParams :: DL.Arguments tps
                       => String
                       -> SL.List Parameter tps
                       -> DL.Function Location tps (SemMD 'Def d ())
                       -> SemMD 'Top d ()
defineOpcodeWithParams name params def =
  defineOpcode name $ do
    locs <- traverseFC (\(Parameter n t r) -> param n t r) params
    DL.applyFunction def locs

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
  origForm <- RWS.get
  let DefState origFormula = origForm
  let modFormula = origFormula { fName = name
                               , fComment = Seq.empty
                               }
  RWS.put (DefState modFormula)
  SemM def
  modForm <- RWS.get
  let DefState forkedFormula = modForm
  RWS.tell (Seq.singleton forkedFormula)
  -- Restore the original formula so that 'defineOpcode' can finish it off
  RWS.put (DefState origFormula)


-- | Add a descriptive comment to the output file
--
-- Each call appends a new comment line.  Individual calls to comment should not
-- contain newlines.
comment :: String -> SemMD 'Def d ()
comment c = modifyFormula $ \f -> f { fComment = fComment f Seq.|> c }

-- | Declare a named parameter; the 'name' string provided is used as
-- the variable name in the produced formula, the 'ty' string
-- specifies the type (strings are types via TypeLits), and the 'ety'
-- specifies the expression type for this parameter.  The result is a
-- Location reference to that parameter.
param :: String -> String -> ExprTypeRepr tp -> SemMD 'Def d (Location tp)
param name ty ety = do
  let p = Parameter { pName = name
                    , pType = ty
                    , pExprTypeRepr = ety
                    }
  modifyFormula $ \f -> f { fOperands = fOperands f Seq.|> Some p }
  return (ParamLoc p)

-- | Mark a parameter as an input
input :: Location tp -> SemMD 'Def d ()
input loc = modifyFormula $ \f -> f { fInputs = Some loc : fInputs f }

-- | Define a location as an expression
defLoc :: (HasCallStack) => Location tp -> Expr tp -> SemMD 'Def d ()
defLoc loc e
  | locationType loc == exprType e = do
      curDefs <- RWS.gets (fDefs . smdFormula)
      case lookup (Some loc) curDefs of
        Nothing -> modifyFormula $ \f -> f { fDefs = (Some loc, Some e) : fDefs f }
        Just _ -> error (printf "Location is already defined: %s" (show loc))
  | otherwise = error (printf "Type mismatch; got %s but expected %s" (show (exprType e)) (show (locationType loc)))

-- Internal helper
modifyFormula :: (Formula d -> Formula d) -> SemMD 'Def d ()
modifyFormula f = RWS.modify' $ \(DefState form) -> DefState (f form)

-- | Define a subphrase that is a natural candidate for a let binding
-- with the associated variable name.
infix 1 =:
(=:) :: String -> Expr tp -> Expr tp
name =: expr = NamedSubExpr name expr


-- | Get the current architecture-specific data in the DSL computation
getArchData :: SemMD 'Def d (Maybe d)
getArchData = fArchData <$> smdFormula <$> RWS.get


-- | Set the current architecture-specific data in the DSL computation
setArchData :: Maybe d -> SemMD 'Def d ()
setArchData m'ad = RWS.modify (\(DefState f) -> DefState (f { fArchData = m'ad }))


-- | Modify the current architecture-specific data in the DSL computation
modifyArchData :: (Maybe d -> Maybe d) -> SemMD 'Def d ()
modifyArchData adf = RWS.modify (\(DefState f) -> DefState (f { fArchData = adf (fArchData f) }))

-- | Create a library function. The function's sexp will be emitted in a separate file.
--
-- Use defined functions for commonly-used sequences to cut down on the sizes of
-- expressions even after conversion to What4.
--
-- > defineLibraryFunction "frob"
-- >   (Arg "x" EInt :<
-- >    Arg "y" (EBV 32) :< Nil) EBool $
-- >   \(x :: Expr 'TInt) (y :: Expr 'TBV) -> ... -- body returns Expr 'TBool
defineLibraryFunction :: forall (args :: [ExprTag]) (tp :: ExprTag)
                       . String
                      -- ^ The name of the function. If there is already a
                      -- function with this name, 'defineLibraryFunction' does
                      -- nothing.
                      -> SL.List Argument args
                      -- ^ The name and type for each argument to the function
                      -> DL.Function Expr args (Expr tp)
                      -- ^ A function from expressions representing the
                      -- arguments to an expression for the body. The arity of
                      -- the function is determined by @sig@.
                      -> LibraryFunctionDef '(args, tp)
defineLibraryFunction name args f =
  LibFuncDef { lfdName = name
             , lfdArgs = args
             , lfdRetType = exprType body
             , lfdBody = body }
  where
    body = DL.applyFunction f argExprs
    argExprs = fmapFC (\arg -> Loc (ParamLoc (toParam arg))) args

    toParam :: forall argTp. Argument argTp -> Parameter argTp
    toParam (Arg argName exprTp) =
      Parameter { pName = argName
                , pType = "<unknown>" -- not used for anything
                , pExprTypeRepr = exprTp }

-- | Wrap a function as a library function. Defines the function and applies it
-- to the given arguments in one go. Convenient for making library functions out
-- of existing code.
--
-- > frob :: Expr 'BV -> 'Expr Bool -> 'Expr Int
-- > frob = wrapAsLibraryFunction "frob"
-- >          (Arg "x" (EBV 32) :< Arg "y" EBool :< Nil) $
-- >          \x y -> ... -- original code for function body
wrapAsLibraryFunction :: forall (args :: [ExprTag]) (tp :: ExprTag)
                       . DL.Arguments args
                      => String
                      -> SL.List Argument args
                      -> DL.Function Expr args (Expr tp)
                      -> DL.Function Expr args (Expr tp)
wrapAsLibraryFunction name formalArgs f =
  DL.function $ \actualArgs ->
    let lfd :: LibraryFunctionDef '(args, tp)
        lfd = defineLibraryFunction name formalArgs f
    in lf lfd actualArgs

-- ----------------------------------------------------------------------
-- Expressions

-- | Allow for user-defined uninterpreted functions over expressions
uf :: ExprTypeRepr tp -> String -> [Some Expr] -> Expr tp
uf = UninterpretedFunc

-- | Allow for user-defined uninterpreted functions over locations
locUF :: ExprTypeRepr tp -> String -> Location tp' -> Location tp
locUF = LocationFunc

-- | Allow for library functions over expressions

lf :: LibraryFunctionDef '(args, ret) -> SL.List Expr args -> Expr ret
lf = LibraryFunc

-- | Unpack a specific operand type using an architecture-specific
-- uninterpreted function
unpackUF :: String -> ExprTypeRepr tp -> String -> Location 'TPackedOperand -> Expr tp
unpackUF operandName rtype ufname fromParam =
    case exprType (Loc fromParam) of
      EPackedOperand o | operandName == o -> uf rtype ufname [Some $ Loc fromParam]
                       | otherwise -> error $ ufname <> " expected a \"" <> operandName <> "\" but got a " <> o

-- | Unpack a specific operand type using an architecture-specific
-- undefined function
unpackLocUF :: String -> ExprTypeRepr tp -> String -> Location 'TPackedOperand -> Location tp
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

anyp :: (HasCallStack) => [Expr 'TBool] -> Expr 'TBool
anyp [] = LitBool False
anyp (r : rs) = orp r (anyp rs)

allp :: (HasCallStack) => [Expr 'TBool] -> Expr 'TBool
allp [] = LitBool True
allp (r : rs) = andp r (allp rs)

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

-- | Test if a specific bit is set
testBit :: Int -> Expr 'TBV -> Expr 'TBool
testBit n = bveq (LitBV 1 1) . extract n n

-- | Test a dynamically-chosen bit number (i.e., the bit number to test is an
-- expr and not an 'Int').
testBitDynamic ::
  HasCallStack =>
  Expr 'TBV {-^ bitvector to test -} ->
  Expr 'TBV {-^ index, with 0 being least significant bit -} ->
  Expr 'TBool
testBitDynamic expr index
  | wExpr == wIndex = testBit (wExpr-1) (bvlshr expr index)
  | otherwise = error (printf "Type mismatch in testBitDynamic: arguments must have same bit width, but saw widths %d and %d" wExpr wIndex)
  where
  EBV wExpr  = exprType expr
  EBV wIndex = exprType index

testBitDynamic32 :: HasCallStack => Expr 'TBV -> Expr 'TBV -> Expr 'TBool
testBitDynamic32 expr index
  | wExpr == 32 && wIndex == 32 = testBitDynamic expr index
  | otherwise = error (printf "Type mismatch in testBitDynamic32: expected two 32-bit arguments, but saw widths %d and %d" wExpr wIndex)
  where
  EBV wExpr  = exprType expr
  EBV wIndex = exprType index

testBitDynamic64 :: HasCallStack => Expr 'TBV -> Expr 'TBV -> Expr 'TBool
testBitDynamic64 expr index
  | wExpr == 64 && wIndex == 64 = testBitDynamic expr index
  | otherwise = error (printf "Type mismatch in testBitDynamic64: expected two 64-bit arguments, but saw widths %d and %d" wExpr wIndex)
  where
  EBV wExpr  = exprType expr
  EBV wIndex = exprType index


-- | The extract operation defined on bitvectors in SMTLib
--
-- Checks to ensure that the requested bits are in bounds and marks the size of
-- the new bitvector.
--
-- Takes arguments using the PPC specification's bit numbering (0 is the most
-- significant bit) and converts it to SMTLib numbering (0 is the least
-- significant bit).
--
-- If you call @extract i j bv@ and @bv@ has bit-width @m@, the SMTLib
-- operation is:
--
-- >      ((_ extract (m-1 - j) (m-1 - i)) (_ BitVec m) (_ BitVec n))
-- >    where
-- >    - i, j, m, n are numerals
-- >    - m > i ≥ j ≥ 0,
-- >    - n = i - j + 1
--
-- We only allow zero length sequences specified as @extract' (j-1) j
-- e@, to avoid masking bugs that manifest as negative size
-- slices. For zero length, we return a zero-length literal, instead
-- of calling the underlying SMTLib @extract@.
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
    EBV m ->
      let n = i - j + 1
          i' = Some . LitInt . fromIntegral $ m-1 - j
          j' = Some . LitInt . fromIntegral $ m-1 - i
      in case m > i && i >= j - 1 && j >= 0 of
        True | n == 0    -> LitBV 0 0
             | otherwise -> TheoryFunc (EBV n) "extract" [i', j'] [Some e]
        False -> error (printf "Invalid slice (%d,%d) of a %d-bit vector" i j m)

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

-- | Zero extend a value so that it has the specified width
zeroExtendTo :: (HasCallStack)
             => Int
             -- ^ The number of bits to extend to
             -> Expr 'TBV
             -- ^ The expression to extend
             -> Expr 'TBV
zeroExtendTo fullWidth e
  | extendBy == 0 = e
  | otherwise = zeroExtend extendBy e
  where
    extendBy = fullWidth - exprBVSize e

-- | Abbreviation for 'zeroExtendTo'
zext' :: (HasCallStack) => Int -> Expr 'TBV -> Expr 'TBV
zext' = zeroExtendTo

signExtend :: (HasCallStack)
           => Int
           -- ^ The number of bits to extend by
           -> Expr 'TBV
           -- ^ The expression to extend
           -> Expr 'TBV
signExtend n e =
  case exprType e of
    EBV w -> TheoryFunc (EBV (w + n)) "sign_extend" [Some (LitInt (fromIntegral n))] [Some e]

signExtendTo :: (HasCallStack)
             => Int
             -- ^ The number of bits to extend to
             -> Expr 'TBV
             -- ^ The expression to extend
             -> Expr 'TBV
signExtendTo fullWidth e
  | extendBy == 0 = e
  | otherwise = signExtend extendBy e
  where
    extendBy = fullWidth - exprBVSize e

-- | Abbreviation for 'signExtendTo'
sext' :: (HasCallStack) => Int -> Expr 'TBV -> Expr 'TBV
sext' = signExtendTo

-- | Concatenate two bitvectors
concat :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
concat e1 e2 =
  case (exprType e1, exprType e2) of
    (EBV 0, EBV 0) -> LitBV 0 0
    (EBV 0, _) -> e2
    (_, EBV 0) -> e1
    -- Without the above special cases for zero length bit vectors,
    -- the "builtin concat" here produces wrong results, apparently
    -- treating zero length bit vectors as length one!?!?
    (EBV w1, EBV w2) -> Builtin (EBV (w1 + w2)) "concat" [Some e1, Some e2]

-- | Set bits, specifying the list of bit numbers to set (0-based)
bvset :: [Int] -> Expr 'TBV -> Expr 'TBV
bvset bitnums e = LitBV n mask `bvor` e
  where
    n = exprBVSize e
    mask = foldl B.setBit 0 bitnums

-- | Clear bits, specifying the list of bit numbers to clear (0-based)
bvclr :: [Int] -> Expr 'TBV -> Expr 'TBV
bvclr bitnums e = LitBV n mask `bvand` e
  where
    n = exprBVSize e
    -- Note that Data.Bits considers -1 as an Integer to be an infinite stream
    -- of 1 bits
    mask = foldl B.clearBit (-1 :: Integer) bitnums B..&. ones
    ones = (1 `B.shiftL` n) - 1

bvPredToBit :: Expr 'TBool -> Expr 'TBV
bvPredToBit p = ite p (LitBV 1 1) (LitBV 1 0)

bvZeros :: Int -> Expr 'TBV
bvZeros n = LitBV n 0

bvOnes :: Int -> Expr 'TBV
bvOnes n = LitBV n $ (1 `B.shiftL` n) - 1

-- | bvUpdate x i y ::= x[i : (i + size(y))] <- y
bvUpdate :: Expr 'TBV -> Int -> Expr 'TBV -> Expr 'TBV
bvUpdate x i y
  | i + y_size <= x_size
  = let clear_mask = concat (concat (bvOnes i) (bvZeros y_size))
                            (bvOnes (x_size - i - y_size))
        set_mask =
          concat (concat (bvZeros i) y) (bvZeros (x_size - i - y_size))
    in  bvor (bvand x clear_mask) set_mask
  | otherwise
  = error $ printf "Out of bounds x(%d)[%d : %d] <- y(%d)"
                   x_size
                   i
                   (i + y_size - 1)
                   y_size
 where
  x_size = exprBVSize x
  y_size = exprBVSize y

-- | bvUpdate' x i y ::= x[i : (i + size(y))] <- y
bvUpdate' :: Expr 'TBV -> Expr 'TBV -> Expr 'TBV -> Expr 'TBV
bvUpdate' x i y =
  let clear_mask =
        bvnot (bvlshr (concat (bvOnes y_size) (bvZeros (x_size - y_size))) i)
      set_mask = bvlshr (concat y (bvZeros (x_size - y_size))) i
  in  bvor (bvand x clear_mask) set_mask
 where
  x_size = exprBVSize x
  y_size = exprBVSize y


-- ----------------------------------------------------------------------
-- Floating-point operations

fpDoubleToSingle :: (HasCallStack) => Expr 'TDouble -> Expr 'TFloat
fpDoubleToSingle e = uf EFloat "fp.double_to_single" [ Some e ]

fpSingleToDouble :: (HasCallStack) => Expr 'TFloat -> Expr 'TDouble
fpSingleToDouble e = Builtin EDouble "fp_single_to_double" [ Some e ]

fpBinaryToDouble :: (HasCallStack) => Expr 'TBV -> Expr 'TDouble
fpBinaryToDouble e
  | expectedType == actualType = Builtin EDouble "fp_binary_to_double" [Some e]
  | otherwise = error
    (printf "Type mismatch: expected %s but got %s"
            (show expectedType)
            (show actualType)
    )
 where
  expectedType = EBV 64
  actualType   = exprType e

fpBinaryToSingle :: (HasCallStack) => Expr 'TBV -> Expr 'TFloat
fpBinaryToSingle e
  | expectedType == actualType = Builtin EFloat "fp_binary_to_single" [Some e]
  | otherwise = error
    (printf "Type mismatch: expected %s but got %s"
            (show expectedType)
            (show actualType)
    )
 where
  expectedType = EBV 32
  actualType   = exprType e

fpDoubleToBinary :: (HasCallStack) => Expr 'TDouble -> Expr 'TBV
fpDoubleToBinary e = Builtin (EBV 64) "fp_double_to_binary" [ Some e ]

fpSingleToBinary :: (HasCallStack) => Expr 'TFloat -> Expr 'TBV
fpSingleToBinary e = Builtin (EBV 32) "fp_single_to_binary" [ Some e ]

fnegd :: (HasCallStack) => Expr 'TDouble -> Expr 'TDouble
fnegd = floatArithUnBuiltinWoRounding "fnegd"

fnegs :: (HasCallStack) => Expr 'TFloat -> Expr 'TFloat
fnegs = floatArithUnBuiltinWoRounding "fnegs"

fabsd :: (HasCallStack) => Expr 'TDouble -> Expr 'TDouble
fabsd = floatArithUnBuiltinWoRounding "fabsd"

fabss :: (HasCallStack) => Expr 'TFloat -> Expr 'TFloat
fabss = floatArithUnBuiltinWoRounding "fabss"

fsqrt :: (HasCallStack) => Expr 'TBV -> Expr 'TDouble -> Expr 'TDouble
fsqrt = floatArithUnBuiltin "fsqrt"

fsqrts :: (HasCallStack) => Expr 'TBV -> Expr 'TFloat -> Expr 'TFloat
fsqrts = floatArithUnBuiltin "fsqrts"

fadd
  :: (HasCallStack)
  => Expr 'TBV
  -> Expr 'TDouble
  -> Expr 'TDouble
  -> Expr 'TDouble
fadd = floatArithBinBuiltin "fadd"

fadds
  :: (HasCallStack)
  => Expr 'TBV
  -> Expr 'TFloat
  -> Expr 'TFloat
  -> Expr 'TFloat
fadds = floatArithBinBuiltin "fadds"

fsub
  :: (HasCallStack)
  => Expr 'TBV
  -> Expr 'TDouble
  -> Expr 'TDouble
  -> Expr 'TDouble
fsub = floatArithBinBuiltin "fsub"

fsubs
  :: (HasCallStack)
  => Expr 'TBV
  -> Expr 'TFloat
  -> Expr 'TFloat
  -> Expr 'TFloat
fsubs = floatArithBinBuiltin "fsubs"

fmul
  :: (HasCallStack)
  => Expr 'TBV
  -> Expr 'TDouble
  -> Expr 'TDouble
  -> Expr 'TDouble
fmul = floatArithBinBuiltin "fmul"

fmuls
  :: (HasCallStack)
  => Expr 'TBV
  -> Expr 'TFloat
  -> Expr 'TFloat
  -> Expr 'TFloat
fmuls = floatArithBinBuiltin "fmuls"

fdiv
  :: (HasCallStack)
  => Expr 'TBV
  -> Expr 'TDouble
  -> Expr 'TDouble
  -> Expr 'TDouble
fdiv = floatArithBinBuiltin "fdiv"

fdivs
  :: (HasCallStack)
  => Expr 'TBV
  -> Expr 'TFloat
  -> Expr 'TFloat
  -> Expr 'TFloat
fdivs = floatArithBinBuiltin "fdivs"

ffma
  :: (HasCallStack)
  => Expr 'TBV
  -> Expr 'TDouble
  -> Expr 'TDouble
  -> Expr 'TDouble
  -> Expr 'TDouble
ffma = floatArithTernBuiltin "ffma"

ffmas
  :: (HasCallStack)
  => Expr 'TBV
  -> Expr 'TFloat
  -> Expr 'TFloat
  -> Expr 'TFloat
  -> Expr 'TFloat
ffmas = floatArithTernBuiltin "ffmas"

fltd :: (HasCallStack) => Expr 'TDouble -> Expr 'TDouble -> Expr 'TBool
fltd = floatLogicBinBuiltin "fltd"

flts :: (HasCallStack) => Expr 'TFloat -> Expr 'TFloat -> Expr 'TBool
flts = floatLogicBinBuiltin "flts"

feqd :: (HasCallStack) => Expr 'TDouble -> Expr 'TDouble -> Expr 'TBool
feqd = floatLogicBinBuiltin "feqd"

feqs :: (HasCallStack) => Expr 'TFloat -> Expr 'TFloat -> Expr 'TBool
feqs = floatLogicBinBuiltin "feqs"

fled :: (HasCallStack) => Expr 'TDouble -> Expr 'TDouble -> Expr 'TBool
fled = floatLogicBinBuiltin "fled"

fles :: (HasCallStack) => Expr 'TFloat -> Expr 'TFloat -> Expr 'TBool
fles = floatLogicBinBuiltin "fles"

fnand :: (HasCallStack) => Expr 'TDouble -> Expr 'TBool
fnand = floatLogicUnBuiltin "fnand"

fnans :: (HasCallStack) => Expr 'TFloat -> Expr 'TBool
fnans = floatLogicUnBuiltin "fnans"

frsp :: (HasCallStack) => Expr 'TBV -> Expr 'TDouble -> Expr 'TFloat
frsp = floatConvBuiltin "frsp" EDouble EFloat

fctid :: (HasCallStack) => Expr 'TBV -> Expr 'TDouble -> Expr 'TBV
fctid = floatConvBuiltin "fctid" EDouble (EBV 64)

fctidu :: (HasCallStack) => Expr 'TBV -> Expr 'TDouble -> Expr 'TBV
fctidu = floatConvBuiltin "fctidu" EDouble (EBV 64)

fctiw :: (HasCallStack) => Expr 'TBV -> Expr 'TDouble -> Expr 'TBV
fctiw = floatConvBuiltin "fctiw" EDouble (EBV 32)

fctiwu :: (HasCallStack) => Expr 'TBV -> Expr 'TDouble -> Expr 'TBV
fctiwu = floatConvBuiltin "fctiwu" EDouble (EBV 32)

fcfid :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TDouble
fcfid = floatConvBuiltin "fcfid" (EBV 64) EDouble

fcfids :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TFloat
fcfids = floatConvBuiltin "fcfids" (EBV 64) EFloat

fcfidu :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TDouble
fcfidu = floatConvBuiltin "fcfidu" (EBV 64) EDouble

fcfidus :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TFloat
fcfidus = floatConvBuiltin "fcfidus" (EBV 64) EFloat

frti :: (HasCallStack) => Expr 'TBV -> Expr 'TDouble -> Expr 'TDouble
frti = floatConvBuiltin "frti" EDouble EDouble

frtis :: (HasCallStack) => Expr 'TBV -> Expr 'TFloat -> Expr 'TFloat
frtis = floatConvBuiltin "frtis" EFloat EFloat

floatArithTernBuiltin
  :: (HasCallStack)
  => String
  -> Expr 'TBV
  -> Expr tp
  -> Expr tp
  -> Expr tp
  -> Expr tp
floatArithTernBuiltin s r e1 e2 e3
  | t1 == t2, t1 == t3, EBV 2 == exprType r =
    Builtin t1 s [Some r, Some e1, Some e2, Some e3]
  | otherwise = error
    (printf
      "Type mismatch for ternary float builtin %s: rounding mode type is %s, arg1 type is %s, arg2 type is %s, arg3 type is %s"
      s
      (show $ exprType r)
      (show t1)
      (show t2)
      (show t3)
    )
 where
  t1 = exprType e1
  t2 = exprType e2
  t3 = exprType e3

floatArithBinBuiltin
  :: (HasCallStack) => String -> Expr 'TBV -> Expr tp -> Expr tp -> Expr tp
floatArithBinBuiltin s r e1 e2
  | t1 == t2, EBV 2 == exprType r = Builtin t1 s [Some r, Some e1, Some e2]
  | otherwise = error
    (printf
      "Type mismatch for binary float builtin %s: rounding mode type is %s, lhs type is %s while rhs type is %s"
      s
      (show $ exprType r)
      (show t1)
      (show t2)
    )
 where
  t1 = exprType e1
  t2 = exprType e2

floatArithUnBuiltin
  :: (HasCallStack) => String -> Expr 'TBV -> Expr tp -> Expr tp
floatArithUnBuiltin s r e
  | EBV 2 == exprType r = Builtin (exprType e) s [Some r, Some e]
  | otherwise = error
    (printf
      "Type mismatch for unary float builtin %s: rounding mode type is %s"
      s
      (show $ exprType r)
    )

floatArithUnBuiltinWoRounding :: (HasCallStack) => String -> Expr tp -> Expr tp
floatArithUnBuiltinWoRounding s e = Builtin (exprType e) s [Some e]

floatLogicBinBuiltin
  :: (HasCallStack) => String -> Expr tp -> Expr tp -> Expr 'TBool
floatLogicBinBuiltin s e1 e2
  | t1 == t2 = Builtin EBool s [Some e1, Some e2]
  | otherwise = error
    (printf
      "Type mismatch for binary float builtin %s; lhs type is %s while rhs type is %s"
      s
      (show t1)
      (show t2)
    )
 where
  t1 = exprType e1
  t2 = exprType e2

floatLogicUnBuiltin :: (HasCallStack) => String -> Expr tp -> Expr 'TBool
floatLogicUnBuiltin s e = Builtin EBool s [Some e]

floatConvBuiltin
  :: (HasCallStack)
  => String
  -> ExprTypeRepr tp'
  -> ExprTypeRepr tp
  -> Expr 'TBV
  -> Expr tp'
  -> Expr tp
floatConvBuiltin s from_type to_type r expr
  | from_type == expr_type, EBV 2 == exprType r =
    Builtin to_type s [Some r, Some expr]
  | otherwise = error
    (printf
      "Unexpected argument type for float conversion builtin %s; expected %s, found %s"
      s
      (show from_type)
      (show expr_type)
    )
 where
  expr_type = exprType expr

-- ----------------------------------------------------------------------
-- SExpression conversion

mkSExprs :: Seq.Seq (Formula d) -> ([(String, Definition)], M.Map String FunctionDefinition)
mkSExprs formulas = (map toSExpr list, gatherFunSExprs list)
  where
    list = F.toList formulas

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
      fromFoldable' (fromFoldable' [ident "_", ident "call", string ("uf." ++ name)] : map convertExpr params)
    LibraryFunc (LibFuncDef { lfdName = name }) params ->
      fromFoldable' (fromFoldable' [ident "_", ident "call", string ("df." ++ name)] : map convertExpr (toListFC Some params))
    NamedSubExpr name expr ->
        let tag d = \case
                    SC.SNil -> SC.SNil  -- no tagging of nil elements
                    SC.SAtom a -> SC.SAtom $ ANamed name d a
                    SC.SCons l r -> SC.SCons (tag (d+1) l) r
        in tag 0 $ convertExpr $ Some expr
    PackedOperand name -> error ("PackedOperand " <> name <>
                                 " not unpacked with unpackUF.. cannot serialize")

convertLoc :: Location tp -> SC.SExpr FAtom
convertLoc loc =
  case loc of
    ParamLoc p -> ident (pName p)
    LiteralLoc ll -> quoted $ lName ll
    MemoryLoc _ident -> quoted "Mem"
    LocationFunc _ func loc' ->
      fromFoldable' [fromFoldable' [ident "_", ident "call", string ("uf." ++ func)], convertLoc loc']

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

-- ----------------------------------------------------------------------
-- SExpression conversion for library functions

gatherFunSExprs :: [Formula d] -> M.Map String FunctionDefinition
gatherFunSExprs = M.map (viewSome funToSExpr) . gatherFunctions

funToSExpr :: LibraryFunctionDef sig -> FunctionDefinition
funToSExpr lfd@(LibFuncDef {}) =
  FunctionDefinition $ extractFunSExpr (lfdName lfd) (lfdArgs lfd) (lfdRetType lfd) (lfdBody lfd)

extractFunSExpr :: String -> SL.List Argument args -> ExprTypeRepr ret
                -> Expr ret -> SC.SExpr FAtom
extractFunSExpr name args retType body =
  fromFoldable' [ fromFoldable' [ ident "function", ident name ]
                , fromFoldable' [ ident "arguments", convertArguments args ]
                , fromFoldable' [ ident "return", convertExprType retType ]
                , fromFoldable' [ ident "body", convertExpr (Some body) ]
                ]

-- Subtle difference between this and 'convertOperands': Here we output the
-- *base* type rather than the architecture-specific symbol
convertArguments :: SL.List Argument tps -> SC.SExpr FAtom
convertArguments =
  fromFoldable' . toListFC argToDecl
  where
    argToDecl (Arg name tp) =
      fromFoldable' [ ident name, convertExprType tp ]

convertExprType :: ExprTypeRepr tp -> SC.SExpr FAtom
convertExprType = viewSome convertBaseType . exprTypeToBaseType

exprTypeToBaseType :: ExprTypeRepr tp -> Some CRU.BaseTypeRepr
exprTypeToBaseType repr =
  case repr of
    EBool -> Some CRU.BaseBoolRepr
    EInt -> Some CRU.BaseIntegerRepr
    EBV n | Just (Some n') <- NR.someNat @Integer (fromIntegral n)
          , Just NR.LeqProof <- NR.isPosNat n'
          -> Some (CRU.BaseBVRepr n')
    EFloat -> Some $
      CRU.BaseFloatRepr (CRU.knownRepr :: CRU.FloatPrecisionRepr CRU.Prec32)
    EDouble -> Some $
      CRU.BaseFloatRepr (CRU.knownRepr :: CRU.FloatPrecisionRepr CRU.Prec64)
    _ -> error $ "cannot convert to What4 type: " ++ show repr

-- This supports all base types, not just those that we convert from ExprTypes,
-- as a historical accident.
convertBaseType :: CRU.BaseTypeRepr tp -> SC.SExpr FAtom
convertBaseType repr =
  case repr of
    CRU.BaseBoolRepr -> quoted "bool"
    CRU.BaseBVRepr n ->
      fromFoldable' [ quoted "bv", int (fromIntegral (CRU.natValue n)) ]
    CRU.BaseNatRepr -> quoted "nat"
    CRU.BaseIntegerRepr -> quoted "int"
    CRU.BaseRealRepr -> quoted "real"
    CRU.BaseFloatRepr (CRU.FloatingPointPrecisionRepr eb sb) -> fromFoldable'
      [ quoted "fp"
      , int (fromIntegral (CRU.natValue eb))
      , int (fromIntegral (CRU.natValue sb))
      ]
    CRU.BaseStringRepr _ -> quoted "string"
    CRU.BaseComplexRepr -> quoted "complex"
    CRU.BaseStructRepr reprs ->
      fromFoldable' [ quoted "struct", fromFoldable' (toListFC convertBaseType reprs) ]
    CRU.BaseArrayRepr ixs elt ->
      fromFoldable' [ quoted "array", fromFoldable' (toListFC convertBaseType ixs),
                      convertBaseType elt ]

printFunctionDefinition :: FunctionDefinition -> T.Text
printFunctionDefinition (FunctionDefinition sexpr) = printTokens Seq.empty sexpr

-- ----------------------------------------------------------------------
-- Gather all library functions from the given formulas

gatherFunctions :: [Formula d] -> M.Map String (Some LibraryFunctionDef)
gatherFunctions = F.foldMap doFormula
  -- Note that the Monoid instance for M.Map is a left-biased union, so the first
  -- definition will be used and the second thrown away
  where
    doFormula f = F.foldMap (doSomeExpr . snd) (fDefs f)

    doSomeExpr :: Some Expr -> M.Map String (Some LibraryFunctionDef)
    doSomeExpr (Some expr) = doExpr expr

    doExpr :: Expr tp -> M.Map String (Some LibraryFunctionDef)
    doExpr expr = case expr of
      LibraryFunc def args ->
        M.insertWith keepOld (lfdName def) (Some def) $ foldMapFC doExpr args
      Builtin _ _ args ->
        foldMap doSomeExpr args
      TheoryFunc _ _ conParams appParams ->
        foldMap doSomeExpr (conParams ++ appParams)
      UninterpretedFunc _ _ params ->
        foldMap doSomeExpr params
      NamedSubExpr _ e -> doExpr e
      LitBool _ -> M.empty
      LitBV _ _ -> M.empty
      LitInt _ -> M.empty
      LitString _ -> M.empty
      Loc _ -> M.empty
      PackedOperand _ -> M.empty

    keepOld _new old = old
