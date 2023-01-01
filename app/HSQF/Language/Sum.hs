{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveAnyClass #-}
module HSQF.Language.Sum () where

import Generics.SOP
import HSQF.Prelude
import Data.Kind (Type)
import Generics.SOP.NP (trans_NP)
import Generics.SOP.Constraint (Tail, Head)
import HSQF.Language.Definition (Term(runTerm, MkTerm))
import SQF (SQF(ListLit, NumLit))
import qualified GHC.Generics as GHC

newtype DerivePSumViaData (pa :: PType) (s :: S) = MkDeriveViaData (pa s)

undefined' :: forall a. a
undefined' = undefined

fromInt :: Int -> Float
fromInt = fromIntegral -- need due GHC bug

data ExampleSumType (s :: S)
  = TheOnlyOne (Term 'Expr s PInteger)
  | SecondOne (Term 'Expr s PString)
  deriving stock GHC.Generic
  deriving anyclass Generic

case1 :: ExampleSumType s
case1 = TheOnlyOne (pconstant @Integer 200)

case2 :: ExampleSumType s
case2 = SecondOne (pconstant "Some string")

compiledCase1 = gpcon (MkDeriveViaData case1)
compiledCase2 = gpcon (MkDeriveViaData case2)
-- >>> (compile compiledCase1, compile compiledCase2)
-- ("[0.0,200.0];","[1.0,\"Some string\"];")

gpcon :: forall (pa :: PType) (pass :: [PType]) (ass :: [[Type]]) s c.
  ( Generic (pa s)
  , ass ~ Code (pa s)
  , AllZip (IsSingletonProduct s) ass pass
  ) => DerivePSumViaData pa s -> Term c s (DerivePSumViaData pa)
gpcon (MkDeriveViaData x) = unwrap @ass @pass (from x) 0
  where
    unwrap :: forall (xss :: [[Type]]) (pxs :: [PType]).
      AllZip (IsSingletonProduct s) xss pxs =>
      SOP I xss -> Int -> Term c s (DerivePSumViaData pa)
    unwrap (SOP y) (conId :: Int) = case y of
      (Z (I caseTerm :* Nil)) -> MkTerm $ \lvl -> 
        let compiled = runTerm caseTerm lvl
            compiledConId = NumLit $ fromInt conId
        in ListLit [compiledConId, compiled]
      (S (cases :: NS (NP I) xss')) ->
        unwrap @xss' @(Tail pxs) (SOP cases) (succ conId)

class (a ~ Term 'Expr s pa) => IsTerm s a pa
class (xs ~ '[Term 'Expr s pa]) => IsSingletonProduct s xs pa
instance (xs ~ '[Term 'Expr s pa]) => IsSingletonProduct s xs pa
transTerms ::
  forall s (as :: [Type]) (pas :: [PType]).
  AllZip (IsTerm s) as pas =>
  NP I as ->
  NP (Term 'Expr s) pas
transTerms =
  trans_NP
    (Proxy :: Proxy (IsTerm s))
    (\(I x) -> x)
