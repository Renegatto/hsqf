{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ViewPatterns #-}

module HSQF.Language.Sum
  ( GPCon (gpcon),
    GPMatch (gpmatch),
    DeriveGenerically (MkDeriveGenerically),
  )
where

import Data.Kind (Type)
import Generics.SOP
import Generics.SOP.Constraint (Tail)
import Generics.SOP.NP (trans_NP)
import HSQF.Language.Definition (Term (MkTerm, runTerm))
import HSQF.Language.Monadic qualified as P
import HSQF.Language.Procedure (plazy, pswitch, pletf)
import HSQF.Prelude
import SQF (SQF (IntLit, ListLit))
import Data.Coerce (coerce)

-- | Need due GHC bug
undefined' :: forall a. a
undefined' = undefined

-- | Need due GHC bug
fromInt :: Int -> Integer
fromInt = fromIntegral

newtype DeriveGenerically (pa :: PType) (s :: S) =
  MkDeriveGenerically (pa s)

instance GPCon pa => PCon (DeriveGenerically pa) where
  type PConstructed (DeriveGenerically pa) = pa
  pcon :: forall c s. DeriveGenerically pa s -> Term c s pa
  pcon x = coerce $ gpcon @pa (coerce x :: pa s)

instance GPMatch pa => PMatch (DeriveGenerically pa) where
  type PPattern (DeriveGenerically pa) = pa
  match ::
    forall pb c s.
    Term 'Expr s (DeriveGenerically pa) ->
    (pa s -> Term c s pb) ->
    Term c s pb
  match x f =
    gpmatch @pa
      (coerce x :: Term 'Expr s pa)
      (coerce f :: pa s -> Term c s pb)

class GPMatch (pa :: PType) where
  gpmatch :: Term 'Expr s pa -> (pa s -> Term c s b) -> Term c s b
  default gpmatch ::
    forall (pb :: PType) (pas :: [PType]) s c ass.
    ( Generic (pa s),
      ass ~ Code (pa s),
      AllZip (IsSingletonProduct s) ass pas
    ) =>
    Term 'Expr s pa ->
    (pa s -> Term c s pb) ->
    Term c s pb
  gpmatch toMatch f = body
    where
      body = P.do
        toMatchRef <- pletf toMatch
        let toMatch' :: forall a. Term 'Expr s (PHList '[PInteger, a])
            toMatch' = punsafeCoerce toMatchRef
        conPayload <- pletf $ sel @1 toMatch'
        let conId = sel @0 toMatch'

            conPayload' :: forall x. Term 'Expr s x
            conPayload' = punsafeCoerce conPayload

            makeCases ::
              forall (ass' :: [[Type]]) (pas' :: [PType]).
              AllZip (IsSingletonProduct s) ass' pas' =>
              NP (Injection (NP I) ass) ass' ->
              [pa s]
            makeCases Nil = []
            makeCases (Fn con :* (rest :: NP (Injection (NP I) ass) ass'')) =
              let arg :: pa s
                  arg = to $ SOP $ unK $ con (I conPayload' :* Nil)
                  next :: [pa s]
                  next = makeCases @ass'' @(Tail pas') rest
              in arg : next

            cases :: [Term c s pb]
            cases = f <$> makeCases @ass @pas injections

        pswitch
          conId
          [(pconstant n, plazy c) | c <- cases | n <- [0 ..]]
          (Just $ plazy $ ptraceError $ pconstant "No such case Id found")

class GPCon (pa :: PType) where
  gpcon :: pa s -> Term c s pa
  default gpcon ::
    forall pass s c.
    ( Generic (pa s),
      AllZip (IsSingletonProduct s) (Code (pa s)) pass
    ) =>
    pa s ->
    Term c s pa
  gpcon x =
    unwrap @(Code (pa s)) @pass (from x) 0
    where
      unwrap ::
        forall (xss :: [[Type]]) (pxs :: [PType]).
        AllZip (IsSingletonProduct s) xss pxs =>
        SOP I xss ->
        Int ->
        Term c s pa
      unwrap (SOP y) (conId :: Int) = case y of
        (Z (I caseTerm :* Nil)) -> MkTerm $ \lvl ->
          let compiled = runTerm caseTerm lvl
              compiledConId = IntLit $ fromInt conId
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