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
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ParallelListComp #-}
module HSQF.Language.Sum
  ( PCon' (pcon'),
    PMatch' (pmatch),
  )
where

import Generics.SOP
import HSQF.Prelude
import Data.Kind (Type)
import Generics.SOP.NP (trans_NP)
import Generics.SOP.Constraint (Tail)
import HSQF.Language.Definition (Term(runTerm, MkTerm))
import SQF (SQF(ListLit, IntLit))
import qualified HSQF.Language.Monadic as P
import HSQF.Language.Procedure (pswitch, plazy)

-- | Need due GHC bug
undefined' :: forall a. a
undefined' = undefined

-- | Need due GHC bug
fromInt :: Int -> Integer
fromInt = fromIntegral

class PMatch' (pa :: PType) where
  pmatch :: Term 'Expr s pa -> (pa s -> Term c s b) -> Term 'Stat s b
  default pmatch :: 
    forall (pb :: PType) (pas :: [PType]) s c ass.
    ( Generic (pa s)
    , ass ~ Code (pa s)
    , AllZip (IsSingletonProduct s) ass pas
    ) =>
    Term 'Expr s pa ->
    (pa s -> Term c s pb) -> Term 'Stat s pb
  pmatch = gpmatch @pa @pb @pas

class PCon' (a :: PType) where
  pcon' :: a s -> Term c s a
  default pcon' :: 
    forall pass s c.
    ( Generic (a s)
    , AllZip (IsSingletonProduct s) (Code (a s)) pass
    ) => a s -> Term c s a
  pcon' = gpcon @a @pass @s @c 

gpcon ::
  forall (pa :: PType) (pas :: [PType]) s c.
  ( Generic (pa s)
  , AllZip (IsSingletonProduct s) (Code (pa s)) pas
  ) =>
  pa s ->
  Term c s pa
gpcon x =
    unwrap @(Code (pa s)) @pas (from x) 0
  where
    unwrap :: forall (xss :: [[Type]]) (pxs :: [PType]).
      AllZip (IsSingletonProduct s) xss pxs =>
      SOP I xss -> Int -> Term c s pa
    unwrap (SOP y) (conId :: Int) = case y of
      (Z (I caseTerm :* Nil)) -> MkTerm $ \lvl -> 
        let compiled = runTerm caseTerm lvl
            compiledConId = IntLit $ fromInt conId
        in ListLit [compiledConId, compiled]
      (S (cases :: NS (NP I) xss')) ->
        unwrap @xss' @(Tail pxs) (SOP cases) (succ conId)

gpmatch ::
  forall (pa :: PType) (pb :: PType) (pas :: [PType]) s c ass.
  ( Generic (pa s)
  , ass ~ Code (pa s)
  , AllZip (IsSingletonProduct s) ass pas
  ) =>
  Term 'Expr s pa ->
  (pa s -> Term c s pb) -> Term 'Stat s pb
gpmatch toMatch f = body
  where
    body = P.do
      toMatchRef <- plet toMatch
      let toMatch' :: forall a. Term 'Expr s (PHList '[PInteger, a])
          toMatch' = punsafeCoerce toMatchRef 
      conPayload <- plet $ sel @1 toMatch'
      let conId = sel @0 toMatch'
        
          conPayload' :: forall x. Term 'Expr s x
          conPayload' = punsafeCoerce conPayload

          makeCases :: forall (ass' :: [[Type]]) (pas' :: [PType]).
            AllZip (IsSingletonProduct s) ass' pas' =>
            NP (Injection (NP I) ass) ass' -> [pa s]
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
        [ (pconstant n, plazy c) | c <- cases | n <- [0..]]
        (Just $ plazy $ ptraceError $ pconstant "No such case Id found")

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