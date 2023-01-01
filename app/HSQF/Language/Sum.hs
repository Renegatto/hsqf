{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
module HSQF.Language.Sum () where

import Generics.SOP
import HSQF.Prelude
import Data.Kind (Type)
import Generics.SOP.NP (trans_NP)

newtype DerivePSumViaData (pa :: PType) (s :: S) = MkDeriveViaData (pa s)

gpcon :: forall (pa :: PType) (pas :: [PType]) (as :: [Type]) s c.
  ( Generic (pa s)
  , '[as] ~ Code (pa s)
  , AllZip (IsTerm s) as pas
  ) => DerivePSumViaData pa s -> Term c s (DerivePSumViaData pa)
gpcon (MkDeriveViaData x) = undefined
  where
    _ = transTerms @s @as @pas
    -- unwrap :: forall xs pxs.
    --   AllZip (IsTerm s) xs pxs =>
    --   SOP I xs -> _
    -- unwrap (SOP y) (n :: Int) = case y of
    --   (Z record)  -> undefined
    --   (S cases) -> unwrap (SOP cases) (succ n)
    _ = case from x of
          SOP (S cases) -> error "Impossible"
          SOP (Z record) -> case record of
            (I (_ :: Term 'Expr s _) :* rest) -> undefined
            Nil -> undefined
undefined' :: forall a. a
undefined' = undefined

gpcon' :: forall (pa :: PType) (pass :: [[PType]]) (ass :: [[Type]]) s c.
  ( Generic (pa s)
  , ass ~ Code (pa s)
  , AllZip (AllZip (IsTerm s)) ass pass
  ) => DerivePSumViaData pa s -> Term c s (DerivePSumViaData pa)
gpcon' (MkDeriveViaData x) = undefined
  where
    _ = case from x of
          SOP (S cases) -> error "Impossible"
          SOP (Z record) -> case record of
            --(I (_ :: Term 'Expr s _) :* rest) -> undefined
            Nil -> undefined

class (a ~ Term 'Expr s pa) => IsTerm s a pa

transTerms ::
  forall s (as :: [Type]) (pas :: [PType]).
  AllZip (IsTerm s) as pas =>
  NP I as ->
  NP (Term 'Expr s) pas
transTerms =
  trans_NP
    (Proxy :: Proxy (IsTerm s))
    (\(I x) -> x)