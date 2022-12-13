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

type ClosedTerm = forall s. Term s

type Term :: S -> PType -> Type
newtype Term s a = MkTerm { runTerm :: Int -> SQF }

-- | SQF Unary Procedure
type (:-->) :: PType -> PType -> PType
newtype (:-->) a b s = MkUProcedure
  { runUProcedure :: Term s (a :--> b)}

-- | SQF NAry Procedure
type (:==>) :: [PType] -> PType -> PType
newtype (:==>) args b s = MkProcedure
  { runProcedure :: Term s (args :==> b)}

(##) :: (forall s0. Term s0 (a :--> b)) -> Term s a -> Term s b
f ## x = MkTerm $ \lvl ->
  Call (runTerm f lvl) (runTerm x lvl)

class PConstant a where
  type PConst a :: PType
  pconstant :: a -> Term s (PConst a)

class PCon (a :: PType) where 
  pcon :: a s -> Term s a

class PMatch (a :: PType) where
  type PPattern a :: PType
  match :: Term s a -> (PPattern a s -> Term s b) -> Term s b

punsafeCoerce :: Term s a -> Term s b
punsafeCoerce = unsafeCoerce

pif :: Term s PBool -> Term s a -> Term s a -> Term s a
pif b success failure = MkTerm $ \lvl ->
  If (runTerm b lvl) (runTerm success lvl) (runTerm failure lvl)

type PInteger :: PType
newtype PInteger s = MkInteger { runPInteger :: Term s PInteger }

instance PCon PInteger where
  pcon :: PInteger s -> Term s PInteger
  pcon = runPInteger

instance PConstant Integer where
  type PConst Integer = PInteger
  pconstant :: Integer -> Term s (PConst Integer)
  pconstant n = MkTerm $ \_ -> NumLit $ fromIntegral n

(#&&) :: Term s PBool -> Term s PBool -> Term s PBool
(#&&) = declareOperator "&&"

(#||) :: Term s PBool -> Term s PBool -> Term s PBool
(#||) = declareOperator "||"

pnot :: Term s PBool -> Term s PBool
pnot = declareUnary "not"

(#==) :: POrd a => Term s a -> Term s a -> Term s PBool
a #== b = (a #>= b) #&& (a #<= b)

(#>) :: POrd a => Term s a -> Term s a -> Term s PBool
a #> b = (a #>= b) #&& pnot (a #== b)

class POrd (a :: PType) where
  {-# MINIMAL (#>=), (#<=) #-}
  (#>=) :: Term s a -> Term s a -> Term s PBool
  (#<=) :: Term s a -> Term s a -> Term s PBool

instance POrd (a :: PType) where
   (#>=) :: Term s a -> Term s a -> Term s PBool
   (#>=) = declareOperator ">="

   (#<=) :: Term s a -> Term s a -> Term s PBool
   (#<=) = declareOperator ">="

instance Num (Term s PInteger) where
  (+) :: Term s PInteger -> Term s PInteger -> Term s PInteger
  (+) = declareOperator "+"
  (-) :: Term s PInteger -> Term s PInteger -> Term s PInteger
  (-) = declareOperator "-"
  (*) :: Term s PInteger -> Term s PInteger -> Term s PInteger
  (*) = declareOperator "*"
  fromInteger :: Integer -> Term s PInteger
  fromInteger = pconstant
  abs :: Term s PInteger -> Term s PInteger
  abs a = pif (a #>= 0) a (negate a)
  signum :: Term s PInteger -> Term s PInteger
  signum a = pif (a #> 0) 1 (pif (a #== 0) 0 (-1))

type PBool :: PType
data PBool s = PTrue | PFalse

mkVar :: Int -> String
mkVar = mappend "var" . show

declareGlobal :: forall a s. String -> Term s a
declareGlobal varid = MkTerm $ \_ -> GlobalVar varid

declareUnary :: forall a b s. String -> Term s a -> Term s b
declareUnary varid x = MkTerm $ \lvl ->
  UnaryOperator varid (runTerm x lvl)

declareOperator :: forall a b c s. String -> Term s a -> Term s b -> Term s c
declareOperator varid x y = MkTerm $ \lvl ->
  BinaryOperator varid (runTerm x lvl) (runTerm y lvl)