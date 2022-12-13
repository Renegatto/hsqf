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

class PConstant pa where
  type PConst a :: Type
  type PUnConst pa :: Type
  pconstant :: PUnConst a -> Term c s pa

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

instance PCon PInteger where
  pcon :: PInteger s -> Term c s PInteger
  pcon n = unExpr $ runPInteger n

instance PConstant PInteger where
  type PUnConst PInteger = Integer
  pconstant :: Integer -> Term c s PInteger
  pconstant n = MkTerm $ \_ -> NumLit $ fromIntegral n

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

mkVar :: Int -> String
mkVar = mappend "var" . show

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