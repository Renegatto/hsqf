{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module PSQF.HList where
import PSQF.Definition
import Data.Kind (Type)
import GHC.TypeNats (KnownNat, type (-))
import USQF
import GHC.TypeLits (natVal)
import Data.Data (Proxy(Proxy))

type PPair :: PType -> PType -> PType
data PPair a b s = MkPPair (Term s a) (Term s b)

-- plet :: Term s a -> (Term s a -> Term s b) -> Term s b
-- plet def scope = MkTerm $ \lvl ->
--   let varid = mkVar lvl
--       def' = runTerm def (succ lvl) 
--       scope' = runTerm (scope $ MkTerm $ \_ -> LocalVar varid) (succ lvl) 
--   in Seq (BindLocally (mkVar lvl) def') scope'

type PTup3 :: PType -> PType -> PType -> PType
data PTup3 a b c s = MkTup3 (Term s a) (Term s b) (Term s c)

-- plam' :: (Term s a -> Term s b) -> Term s (a :--> b)
-- plam' f = MkTerm $ \lvl ->
--   let var = LocalVar $ mkVar lvl
--   in Procedure
--     [ Call (GlobalVar "params") $ ListLit [var]
--     , runTerm (f (MkTerm $ const var)) (succ lvl)
--     ]
type Flip :: (a -> b -> Type) -> b -> a -> Type
newtype Flip f a b = MkFlip (f b a)

ppairToList :: Term s (PPair a b) -> Term s (PHList '[a,b])
ppairToList = punsafeCoerce

instance PCon (PPair a b) where
  pcon :: PPair a b s -> Term s (PPair a b)
  pcon (MkPPair a b) = MkTerm $ \lvl ->
    ListLit [runTerm a lvl, runTerm b lvl]

instance PMatch (PPair a b) where
  type PPattern (PPair a b) = PPair a b
  match :: Term s (PPair a b) -> (PPair a b s -> Term s c) -> Term s c
  match (ppairToList -> p) f = MkTerm $ \lvl ->
    runTerm (f (MkPPair (sel @0 p) (sel @1 p))) lvl

instance PMatch (PHList '[a]) where
  type PPattern (PHList '[a]) = Flip Term a
  match :: Term s (PHList '[a]) -> (Flip Term a s -> Term s c) -> Term s c
  match xs f = MkTerm $ \lvl ->
    runTerm (f $ MkFlip $ sel @0 xs) lvl

instance PMatch (PHList '[a,b]) where
  type PPattern (PHList '[a,b]) = PPair a b
  match :: Term s (PHList '[a,b]) -> (PPair a b s -> Term s c) -> Term s c
  match xs f = MkTerm $ \lvl ->
    runTerm (f $ MkPPair (getFst xs) (getSnd xs)) lvl

instance PMatch (PHList '[a,b,c]) where
  type PPattern (PHList '[a,b,c]) = PTup3 a b c
  match :: Term s (PHList '[a,b,c]) -> (PTup3 a b c s -> Term s d) -> Term s d
  match xs f = MkTerm $ \lvl ->
    runTerm (f $ MkTup3 (sel @0 xs) (sel @1 xs) (sel @2 xs)) lvl

plam ::
  forall args b s.
  PLamL args =>
  (forall s. PPattern (PHList args) s -> Term s b) ->
  Term s (args :==> b)
plam = plam'

class PLamL args where
  plam' ::
    (forall s. PPattern (PHList args) s -> Term s b) ->
    Term s (args :==> b)

instance PLamL '[a] where
  plam' ::
    (forall s. Flip Term a s -> Term s b) ->
    Term s ('[a] :==> b)
  plam' f = MkTerm $ \lvl ->
    let var = LocalVar $ mkVar lvl
    in Procedure
      [ Call (GlobalVar "params") $ ListLit [var]
      , runTerm (f (MkFlip $ MkTerm $ const var)) (succ lvl)
      ]
instance PLamL '[a,b] where
  plam' ::
    (forall s. PPair a b s -> Term s c) ->
    Term s ('[a,b] :==> c)
  plam' f = MkTerm $ \lvl ->
    let var0 = LocalVar $ mkVar lvl
        var1 = LocalVar $ mkVar $ succ lvl
        term = MkTerm . const
    in Procedure
      [ Call (GlobalVar "params") $ ListLit [var0,var1]
      , runTerm (f (MkPPair (term var0) (term var1))) (succ lvl)
      ]
instance PLamL '[a,b,c] where
  plam' ::
    (forall s. PTup3 a b c s -> Term s d) ->
    Term s ('[a,b,c] :==> d)
  plam' f = MkTerm $ \lvl ->
    let var = LocalVar . mkVar
        var0 = var lvl
        var1 = var (lvl + 1)
        var2 = var (lvl + 2)
        term = MkTerm . const
    in Procedure
      [ Call (GlobalVar "params") $ ListLit [var0,var1,var2]
      , runTerm (f (MkTup3 (term var0) (term var1) (term var2))) (succ lvl)
      ]
 
(#) :: (forall s0. Term s0 (args :==> b)) -> Term s (PHList args) -> Term s b
f # x = MkTerm $ \lvl ->
  Call (runTerm f lvl) (runTerm x lvl)

newtype PHList (xs :: [PType]) s = MkHList
  { runHList :: Term s (PHList xs)} 

instance (PConstant a, PConstant b) => PConstant (a,b) where
  type PConst (a,b) = PHList '[PConst a, PConst b]
  pconstant :: (PConstant a, PConstant b) => (a, b) -> Term s (PConst (a, b))
  pconstant (a,b) = ppairList (pconstant a) (pconstant b)


psingleton :: Term s a -> Term s (PHList '[a])
psingleton x = MkTerm $ \lvl ->
  ListLit [runTerm x lvl]

ppairList :: Term s a -> Term s b -> Term s (PHList '[a,b]) 
ppairList a b = MkTerm $ \lvl ->
  ListLit [runTerm a lvl, runTerm b lvl]

-- | Not safe!
select :: forall xs x s. Term s (PHList '[PInteger, PHList xs] :--> x)
select = MkTerm $ \_ -> GlobalVar "select" 

sel ::
  forall n (xs :: [PType]) s.
  KnownNat n =>
  Term s (PHList xs) ->
  Term s (Nth n xs)
sel xs =
    select @xs @(Nth n xs) ## ppairList (pconstant @Integer index) xs
  where
      index = fromIntegral $ natVal $ Proxy @n

getFst :: forall s a b. Term s (PHList '[a,b]) -> Term s a
getFst = sel @0

getSnd :: forall s a b. Term s (PHList '[a,b]) -> Term s b
getSnd = sel @1 

class ToHList (a :: PType) where
  type AsHList a :: [PType]
  toHList :: Term s a -> Term s (PHList (AsHList a))

instance ToHList (PPair a b) where
  type AsHList (PPair a b) = '[a,b]
  toHList :: Term s (PPair a b) -> Term s (PHList '[a,b])
  toHList = punsafeCoerce

instance ToHList (PTup3 a b c) where
  type AsHList (PTup3 a b c) = '[a,b,c]
  toHList :: Term s (PTup3 a b c) -> Term s (PHList '[a,b,c])
  toHList = punsafeCoerce

type family Nth n (xs :: [k]) where
  Nth 0 (x:xs) = x
  Nth n (_:xs) = Nth (n - 1) xs 
