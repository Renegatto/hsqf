{-# LANGUAGE TypeFamilies #-}

module HSQF.Language.Common
  ( -- * Definition
    type S,
    type PType,
    Term,
    (:==>),
    (:-->),
    Scope (Stat, Expr),

    -- * Common
    expr,
    unExpr,
    (##),
    pif,
    ptraceError,

    -- * Typeclasses
    PMatch (PPattern, match),
    PCon (type PConstructed, pcon),
    PConstant (PConstanted, pconstant),
    PLift (PLifted),
    POrd ((#>=), (#<=), (#>), (#<)),
    PEq ((#==)),
    -- * Newtype
    PNewtype,
    pfromNewtype,
    pnewtype,

    -- * Simple built-in datatypes
    PVoid,
    PString,
    PInteger,
    PBool (PTrue, PFalse),
    pnot,
    (#&&),
    (#||),

    -- * Unsafe operations
    punsafeCoerce,
    declareOperator,
    declareUnary,
    declareGlobal,

    -- * Misc
    mkVar,
    var,
    term,
    compile,
  )
where

import Data.Kind (Type)
import HSQF.Language.Definition
  ( PType,
    S,
    Scope (..),
    Term (..),
    type (:-->),
    type (:==>),
  )
import SQF
  ( SQF
      ( BinaryOperator,
        Call,
        GlobalVar,
        If,
        ListLit,
        LocalVar,
        StringLit,
        UnaryOperator,
        IntLit
      ),
    unNewLine,
  )
import SQF qualified (compile)
import Unsafe.Coerce (unsafeCoerce)

newtype PNewtype a s = MkPNewtype (Term 'Expr s a)

pfromNewtype :: Term c s (PNewtype a) -> Term c s a
pfromNewtype = punsafeCoerce

pnewtype :: Term c s a -> Term c s (PNewtype a)
pnewtype = punsafeCoerce

(##) :: Term 'Expr s0 (a :--> b) -> Term 'Expr s a -> Term c s b
f ## x = MkTerm $ \lvl ->
  Call (ListLit [runTerm x lvl]) (runTerm f lvl)

-- We need two classes for better type inference
class (PLifted (PConstanted a) ~ a) => PConstant (a :: Type) where
  type PConstanted a :: PType
  pconstant :: a -> Term c s (PConstanted a)

class (PConstanted (PLifted pa) ~ pa) => PLift (pa :: PType) where
  type PLifted pa :: Type

class PCon (a :: PType) where
  type PConstructed a :: PType
  pcon :: a s -> Term c s (PConstructed a)

class PMatch (a :: PType) where
  type PPattern a :: PType
  match :: Term 'Expr s a -> (PPattern a s -> Term c s b) -> Term c s b

punsafeCoerce :: forall a b s c c'. Term c s a -> Term c' s b
punsafeCoerce = unsafeCoerce

pif :: Term 'Expr s PBool -> Term c s a -> Term c s a -> Term c s a
pif b success failure = MkTerm $ \lvl ->
  If (runTerm b lvl) (runTerm success lvl) (runTerm failure lvl)

type PInteger :: PType
newtype PInteger s = MkInteger {runPInteger :: Term 'Expr s PInteger}

_ = pconstant 2 :: Term c s PInteger

instance PCon PInteger where
  type PConstructed PInteger = PInteger
  pcon :: PInteger s -> Term c s PInteger
  pcon n = unExpr $ runPInteger n

instance PLift PInteger where type PLifted PInteger = Integer

instance PConstant Integer where
  type PConstanted Integer = PInteger
  pconstant :: Integer -> Term c s (PConstanted Integer)
  pconstant n = MkTerm $ \_ -> IntLit $ fromIntegral n

type PString :: PType
newtype PString s = MkString {runPString :: Term 'Expr s PString}

_ = pconstant "foo" :: Term c s PString

type PVoid :: PType
newtype PVoid s = MkPVoid {runPVoid :: Term 'Expr s PVoid}

instance PCon PVoid where
  type PConstructed PVoid = PVoid
  pcon :: PVoid s -> Term c s PVoid
  pcon n = unExpr $ runPVoid n

instance PLift PVoid where type PLifted PVoid = ()

instance PConstant () where
  type PConstanted () = PVoid
  pconstant :: () -> Term c s (PConstanted ())
  pconstant _ = MkTerm $ \_ -> StringLit []

_ = pconstant () :: Term c s PVoid

instance PCon PString where
  type PConstructed PString = PString
  pcon :: PString s -> Term c s PString
  pcon n = unExpr $ runPString n

instance PLift PString where type PLifted PString = String

instance PConstant String where
  type PConstanted String = PString
  pconstant :: String -> Term c s (PConstanted String)
  pconstant s = MkTerm $ \_ -> StringLit s

(#&&) :: Term 'Expr s PBool -> Term 'Expr s PBool -> Term c s PBool
(#&&) = declareOperator "&&"

(#||) :: Term 'Expr s PBool -> Term 'Expr s PBool -> Term c s PBool
(#||) = declareOperator "||"

pnot :: Term 'Expr s PBool -> Term c s PBool
pnot = declareUnary "not"

class PEq (a :: PType) where
  (#==) :: Term 'Expr s a -> Term 'Expr s a -> Term c s PBool
class PEq a => POrd (a :: PType) where
  {-# MINIMAL (#>) #-}
  (#>) :: Term 'Expr s a -> Term 'Expr s a -> Term c s PBool
  (#<) :: Term 'Expr s a -> Term 'Expr s a -> Term c s PBool
  a #< b = pnot $ (a #== b) #|| (a #> b)
  (#>=) :: Term 'Expr s a -> Term 'Expr s a -> Term c s PBool
  a #>= b = (a #== b) #|| (a #> b)
  (#<=) :: Term 'Expr s a -> Term 'Expr s a -> Term c s PBool
  a #<= b = (a #== b) #|| (a #< b)

instance PEq (a :: PType) where 
  (#==) = declareOperator "=="

instance POrd (a :: PType) where
  (#>) :: Term 'Expr s a -> Term 'Expr s a -> Term c s PBool
  (#>) = declareOperator ">"

  (#<) :: Term 'Expr s a -> Term 'Expr s a -> Term c s PBool
  (#<) = declareOperator "<"

instance Num (Term 'Expr s PInteger) where
  (+) :: Term 'Expr s PInteger -> Term 'Expr s PInteger -> Term 'Expr s PInteger
  (+) = declareOperator "+"
  (-) :: Term 'Expr s PInteger -> Term 'Expr s PInteger -> Term 'Expr s PInteger
  (-) = declareOperator "-"
  (*) :: Term 'Expr s PInteger -> Term 'Expr s PInteger -> Term 'Expr s PInteger
  (*) = declareOperator "*"
  fromInteger :: Integer -> Term 'Expr s PInteger
  fromInteger = pconstant
  abs :: Term 'Expr s PInteger -> Term 'Expr s PInteger
  abs a = pif (a #>= 0) a (negate a)
  signum :: Term 'Expr s PInteger -> Term 'Expr s PInteger
  signum a = pif (a #> 0) 1 (pif (a #== 0) 0 (-1))

type PBool :: PType
data PBool s = PTrue | PFalse

instance PCon PBool where
  type PConstructed PBool = PBool
  pcon :: PBool s -> Term c s PBool
  pcon PTrue = MkTerm $ \_ -> GlobalVar "true"
  pcon PFalse = MkTerm $ \_ -> GlobalVar "false"

mkVar :: Int -> String
mkVar = mappend "var" . show

var :: Int -> SQF
var = LocalVar . mkVar

term :: SQF -> Term c s a
term = MkTerm . const

ptraceError :: Term 'Expr s PString -> Term c s a
ptraceError = declareUnary "throw"

declareGlobal :: forall a c s. String -> Term c s a
declareGlobal varid = MkTerm $ \_ -> GlobalVar varid

declareUnary :: forall a b c s. String -> Term 'Expr s a -> Term c s b
declareUnary varid x = MkTerm $ \lvl ->
  UnaryOperator varid (runTerm x lvl)

declareOperator :: forall a b d c s. String -> Term 'Expr s a -> Term 'Expr s b -> Term c s d
declareOperator varid x y = MkTerm $ \lvl ->
  BinaryOperator varid (runTerm x lvl) (runTerm y lvl)

unExpr :: Term 'Expr s a -> Term c s a
unExpr = punsafeCoerce

expr :: Term 'Expr s a -> Term 'Expr s a
expr = id

compile :: Term c s a -> String
compile = unNewLine . SQF.compile 0 . flip runTerm 0