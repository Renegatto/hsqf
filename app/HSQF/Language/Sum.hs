{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module HSQF.Language.Sum () where

import Generics.SOP
import HSQF.Prelude
import Data.Kind (Type)
import Generics.SOP.NP (trans_NP)
import Generics.SOP.Constraint (Tail, Head)
import HSQF.Language.Definition (Term(runTerm, MkTerm))
import SQF (SQF(ListLit, NumLit))

newtype DerivePSumViaData (pa :: PType) (s :: S) = MkDeriveViaData (pa s)

gpcon :: forall (pa :: PType) (pas :: [PType]) (as :: [Type]) s c.
  ( Generic (pa s)
  , '[as] ~ Code (pa s)
  , AllZip (IsTerm s) as pas
  ) => DerivePSumViaData pa s -> Term c s (DerivePSumViaData pa)
gpcon (MkDeriveViaData x) = undefined
  where
    _ = transTerms @s @as @pas

    _ = case from x of
          SOP (S cases) -> error "Impossible"
          SOP (Z record) -> case record of
            (I (_ :: Term 'Expr s _) :* rest) -> undefined
            Nil -> undefined
undefined' :: forall a. a
undefined' = undefined

fromInt :: Int -> Float
fromInt = fromIntegral -- need due GHC bug

gpcon' :: forall (pa :: PType) (pass :: [PType]) (ass :: [[Type]]) s c.
  ( Generic (pa s)
  , ass ~ Code (pa s)
  , AllZip (IsSingletonProduct s) ass pass
  ) => DerivePSumViaData pa s -> Term c s (DerivePSumViaData pa)
gpcon' (MkDeriveViaData x) = undefined'
  where
    unwrap :: forall (xss :: [[Type]]) (pxs :: [PType]).
      AllZip (IsSingletonProduct s) xss pxs =>
      SOP I xss -> Int -> Term 'Expr s pa
    unwrap (SOP y) (conId :: Int) = case y of
      (Z (I caseTerm :* Nil)) -> MkTerm $ \lvl -> 
        let compiled = runTerm caseTerm lvl
            compiledConId = NumLit $ fromIntegral conId
        in ListLit [compiledConId, compiled]
      (S (cases :: NS (NP I) xss')) ->
        unwrap @xss' @(Tail pxs) (SOP cases) (succ conId)

    _ = case from x of
          SOP (S _) -> error "Impossible"
          SOP (Z record) -> case record of
            (I (_ :: Term 'Expr s _) :* _) -> undefined'
            Nil -> undefined'

class (a ~ Term 'Expr s pa) => IsTerm s a pa
class (xs ~ '[Term 'Expr s pa]) => IsSingletonProduct s xs pa
transTerms ::
  forall s (as :: [Type]) (pas :: [PType]).
  AllZip (IsTerm s) as pas =>
  NP I as ->
  NP (Term 'Expr s) pas
transTerms =
  trans_NP
    (Proxy :: Proxy (IsTerm s))
    (\(I x) -> x)