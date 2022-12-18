{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}

module PSQF.Definition where

import Data.Kind (Type)
import Data.List (intercalate)
import qualified SQF
import USQF
import GHC.TypeLits (type (-), natVal)
import GHC.TypeNats (KnownNat)
import Data.Data (Proxy(Proxy))
import Unsafe.Coerce (unsafeCoerce)
-- import HSQF (S,SType)
-- import HSQF qualified

data S
type PType = S -> Type

type Scope :: Type
data Scope = Expr | Stat

type ClosedTerm = forall s. Term s

type Term :: Scope -> S -> PType -> Type
newtype Term c s a = MkTerm { runTerm :: Int -> SQF }

-- | SQF Unary Procedure
type (:-->) :: PType -> PType -> PType
newtype (:-->) a b s = MkUProcedure
  { runUProcedure :: forall c. Term c s (a :--> b)}

-- | SQF NAry Procedure
type (:==>) :: [PType] -> PType -> PType
newtype (:==>) args b s = MkProcedure
  { runProcedure :: forall c. Term c s (args :==> b)}

(##) :: (forall s0. Term Expr s0 (a :--> b)) -> Term Expr s a -> Term c s b
f ## x = MkTerm $ \lvl ->
  Call (runTerm f lvl) (runTerm x lvl)

-- We need two classes for better type inference
class (PLifted (PConstanted a) ~ a) => PConstant (a :: Type) where
  type PConstanted a :: PType
  pconstant :: a -> Term c s (PConstanted a)

class (PConstanted (PLifted pa) ~ pa) => PLift (pa :: PType) where
  type PLifted pa :: Type

class PCon (a :: PType) where 
  pcon :: a s -> Term c s a
 
class PMatch (a :: PType) where
  type PPattern a :: PType
  match :: Term Expr s a -> (PPattern a s -> Term c s b) -> Term c s b

punsafeCoerce :: Term c s a -> Term c' s b
punsafeCoerce = unsafeCoerce

pif :: Term Expr s PBool -> Term c s a -> Term c s a -> Term c s a
pif b success failure = MkTerm $ \lvl ->
  If (runTerm b lvl) (runTerm success lvl) (runTerm failure lvl)

type PInteger :: PType
newtype PInteger s = MkInteger { runPInteger :: Term Expr s PInteger }

_ = pconstant 2 :: Term c s PInteger

instance PCon PInteger where
  pcon :: PInteger s -> Term c s PInteger
  pcon n = unExpr $ runPInteger n

instance PLift PInteger where type PLifted PInteger = Integer

instance PConstant Integer where
  type PConstanted Integer = PInteger
  pconstant :: Integer -> Term c s (PConstanted Integer)
  pconstant n = MkTerm $ \_ -> NumLit $ fromIntegral n

type PString :: PType
newtype PString s = MkString { runPString :: Term Expr s PString }

_ = pconstant "foo" :: Term c s PString

instance PCon PString where
  pcon :: PString s -> Term c s PString
  pcon n = unExpr $ runPString n

instance PLift PString where type PLifted PString = String

instance PConstant String where
  type PConstanted String = PString
  pconstant :: String -> Term c s (PConstanted String)
  pconstant s = MkTerm $ \_ -> StringLit s

(#&&) :: Term Expr s PBool -> Term Expr s PBool -> Term c s PBool
(#&&) = declareOperator "&&"

(#||) :: Term Expr s PBool -> Term Expr s PBool -> Term c s PBool
(#||) = declareOperator "||"

pnot :: Term Expr s PBool -> Term c s PBool
pnot = declareUnary "not"

(#==) :: POrd a => Term Expr s a -> Term Expr s a -> Term c s PBool
a #== b = (a #>= b) #&& (a #<= b)

(#>) :: POrd a => Term Expr s a -> Term Expr s a -> Term c s PBool
a #> b = (a #>= b) #&& pnot (a #== b)

class POrd (a :: PType) where
  {-# MINIMAL (#>=), (#<=) #-}
  (#>=) :: Term Expr s a -> Term Expr s a -> Term c s PBool
  (#<=) :: Term Expr s a -> Term Expr s a -> Term c s PBool

instance POrd (a :: PType) where
   (#>=) :: Term Expr s a -> Term Expr s a -> Term c s PBool
   (#>=) = declareOperator ">="

   (#<=) :: Term Expr s a -> Term Expr s a -> Term c s PBool
   (#<=) = declareOperator ">="

instance Num (Term Expr s PInteger) where
  (+) :: Term Expr s PInteger -> Term Expr s PInteger -> Term Expr s PInteger
  (+) = declareOperator "+"
  (-) :: Term Expr s PInteger -> Term Expr s PInteger -> Term Expr s PInteger
  (-) = declareOperator "-"
  (*) :: Term Expr s PInteger -> Term Expr s PInteger -> Term Expr s PInteger
  (*) = declareOperator "*"
  fromInteger :: Integer -> Term Expr s PInteger
  fromInteger = pconstant
  abs :: Term Expr s PInteger -> Term Expr s PInteger
  abs a = pif (a #>= 0) a (negate a)
  signum :: Term Expr s PInteger -> Term Expr s PInteger
  signum a = pif (a #> 0) 1 (pif (a #== 0) 0 (-1))

type PBool :: PType
data PBool s = PTrue | PFalse

instance PCon PBool where
  pcon :: PBool s -> Term c s PBool
  pcon PTrue = MkTerm $ \_ -> GlobalVar "true"
  pcon PFalse = MkTerm $ \_ -> GlobalVar "false"

mkVar :: Int -> String
mkVar = mappend "var" . show

ptraceError :: Term Expr s PString -> Term c s a
ptraceError = declareUnary "throw"

declareGlobal :: forall a c s. String -> Term c s a
declareGlobal varid = MkTerm $ \_ -> GlobalVar varid

declareUnary :: forall a b c c' s. String -> Term Expr s a -> Term c' s b
declareUnary varid x = MkTerm $ \lvl ->
  UnaryOperator varid (runTerm x lvl)

declareOperator :: forall a b d c c' s. String -> Term Expr s a -> Term Expr s b -> Term c' s d
declareOperator varid x y = MkTerm $ \lvl ->
  BinaryOperator varid (runTerm x lvl) (runTerm y lvl)

unExpr :: Term Expr s a -> Term c s a
unExpr = punsafeCoerce

expr :: Term Expr s a -> Term Expr s a
expr = id