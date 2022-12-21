{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module HSQF.Language.HList where

import Data.Data (Proxy (Proxy))
import Data.Kind (Type)
import GHC.Base (Constraint)
import GHC.TypeLits (natVal)
import GHC.TypeNats (KnownNat, type (-))
import HSQF.Language.Common
  ( PBool (PTrue),
    PCon (..),
    PConstant (..),
    PInteger,
    PLift (..),
    PMatch (..),
    PString,
    PType,
    Scope (..),
    Term (..),
    declareOperator,
    mkVar,
    punsafeCoerce,
    type (:==>),
  )
import HSQF.Language.Definition (Term (MkTerm, runTerm))
import SQF
  ( SQF
      ( BindLocally,
        Call,
        GlobalVar,
        ListLit,
        LocalVar,
        Procedure,
        Seq
      ),
  )

-- FIXME: 'params' must be called with String literals, not with vars identifiers

type PPair :: PType -> PType -> PType
data PPair a b s = MkPPair (Term Expr s a) (Term Expr s b)

plet :: Term Expr s a -> (Term Expr s a -> Term c s b) -> Term Stat s b
plet def scope = MkTerm $ \lvl ->
  let varid = mkVar lvl
      def' = runTerm def (succ lvl)
      scope' = runTerm (scope $ MkTerm $ \_ -> LocalVar varid) (succ lvl)
   in Seq (BindLocally (mkVar lvl) def') scope'

type PTup3 :: PType -> PType -> PType -> PType
data PTup3 a b d s = MkTup3 (Term Expr s a) (Term Expr s b) (Term Expr s d)

type Flip :: (a -> b -> Type) -> b -> a -> Type
newtype Flip f a b = MkFlip (f b a)

ppairToList :: Term c s (PPair a b) -> Term c s (PHList '[a, b])
ppairToList = punsafeCoerce

instance PCon (PPair a b) where
  pcon :: PPair a b s -> Term c s (PPair a b)
  pcon (MkPPair a b) = MkTerm $ \lvl ->
    ListLit [runTerm a lvl, runTerm b lvl]

instance PMatch (PPair a b) where
  type PPattern (PPair a b) = PPair a b
  match :: Term Expr s (PPair a b) -> (PPair a b s -> Term c s d) -> Term c s d
  match (ppairToList -> p) f = MkTerm $ \lvl ->
    runTerm (f (MkPPair (sel @0 p) (sel @1 p))) lvl

instance PMatch (PHList '[a]) where
  type PPattern (PHList '[a]) = Flip (Term Expr) a
  match :: Term Expr s (PHList '[a]) -> (Flip (Term Expr) a s -> Term c s d) -> Term c s d
  match xs f = MkTerm $ \lvl ->
    runTerm (f $ MkFlip $ sel @0 xs) lvl

instance PMatch (PHList '[a, b]) where
  type PPattern (PHList '[a, b]) = PPair a b
  match :: Term Expr s (PHList '[a, b]) -> (PPair a b s -> Term c s d) -> Term c s d
  match xs f = MkTerm $ \lvl ->
    runTerm (f $ MkPPair (getFst xs) (getSnd xs)) lvl

instance PMatch (PHList '[a, b, d]) where
  type PPattern (PHList '[a, b, d]) = PTup3 a b d
  match :: Term Expr s (PHList '[a, b, d]) -> (PTup3 a b d s -> Term c s x) -> Term c s x
  match xs f = MkTerm $ \lvl ->
    runTerm (f $ MkTup3 (sel @0 xs) (sel @1 xs) (sel @2 xs)) lvl

plam ::
  forall args b c c' s.
  PLamL args =>
  (forall s. PPattern (PHList args) s -> Term c s b) ->
  Term c' s (args :==> b)
plam = plam'

class PLamL args where
  plam' ::
    (forall s. PPattern (PHList args) s -> Term c s b) ->
    Term c' s (args :==> b)

instance PLamL '[a] where
  plam' ::
    (forall s. Flip (Term Expr) a s -> Term c s b) ->
    Term c' s ('[a] :==> b)
  plam' f = MkTerm $ \lvl ->
    let var = LocalVar $ mkVar lvl
     in Procedure
          [ Call (GlobalVar "params") $ ListLit [var],
            runTerm (f (MkFlip $ MkTerm $ const var)) (succ lvl)
          ]

instance PLamL '[a, b] where
  plam' ::
    (forall s. PPair a b s -> Term c s d) ->
    Term c' s ('[a, b] :==> d)
  plam' f = MkTerm $ \lvl ->
    let var0 = LocalVar $ mkVar lvl
        var1 = LocalVar $ mkVar $ succ lvl
        term = MkTerm . const
     in Procedure
          [ Call (GlobalVar "params") $ ListLit [var0, var1],
            runTerm (f (MkPPair (term var0) (term var1))) (succ lvl)
          ]

instance PLamL '[a, b, c] where
  plam' ::
    (forall s. PTup3 a b c s -> Term e s d) ->
    Term e' s ('[a, b, c] :==> d)
  plam' f = MkTerm $ \lvl ->
    let var = LocalVar . mkVar
        var0 = var lvl
        var1 = var (lvl + 1)
        var2 = var (lvl + 2)
        term = MkTerm . const
     in Procedure
          [ Call (GlobalVar "params") $ ListLit [var0, var1, var2],
            runTerm (f (MkTup3 (term var0) (term var1) (term var2))) (succ lvl)
          ]

(#) :: Term Expr s (args :==> b) -> Term Expr s (PHList args) -> Term c s b
f # x = MkTerm $ \lvl ->
  Call (runTerm x lvl) (runTerm f lvl)

newtype PHList (xs :: [PType]) s = MkHList
  {runHList :: Term Expr s (PHList xs)}

instance (PLift pa, PLift pb) => PLift (PHList '[pa, pb]) where
  type PLifted (PHList '[pa, pb]) = (PLifted pa, PLifted pb)

instance (PConstant a, PConstant b) => PConstant (a, b) where
  type PConstanted (a, b) = PHList '[PConstanted a, PConstanted b]
  pconstant :: (PConstant a, PConstant b) => (a, b) -> Term c s (PConstanted (a, b))
  pconstant (a, b) = ppairList (pconstant a) (pconstant b)

psingleton :: Term c s a -> Term c s (PHList '[a])
psingleton x = MkTerm $ \lvl ->
  ListLit [runTerm x lvl]

ppairList :: Term c s a -> Term c s b -> Term c s (PHList '[a, b])
ppairList a b = MkTerm $ \lvl ->
  ListLit [runTerm a lvl, runTerm b lvl]

-- | Not safe!
select ::
  forall xs x c s.
  Term Expr s (PHList xs) ->
  Term Expr s PInteger ->
  Term c s x
select = declareOperator "select"

sel ::
  forall n (xs :: [PType]) c s.
  KnownNat n =>
  Term Expr s (PHList xs) ->
  Term c s (Nth n xs)
sel xs = xs `select` pconstant @Integer index
  where
    index = fromIntegral $ natVal $ Proxy @n

getFst :: forall c s a b. Term Expr s (PHList '[a, b]) -> Term c s a
getFst = sel @0

getSnd :: forall c s a b. Term Expr s (PHList '[a, b]) -> Term c s b
getSnd = sel @1

class ToHList (a :: PType) where
  type AsHList a :: [PType]
  toHList :: Term c s a -> Term c s (PHList (AsHList a))

instance ToHList (PPair a b) where
  type AsHList (PPair a b) = '[a, b]
  toHList :: Term c s (PPair a b) -> Term c s (PHList '[a, b])
  toHList = punsafeCoerce

instance ToHList (PTup3 a b d) where
  type AsHList (PTup3 a b d) = '[a, b, d]
  toHList :: Term c s (PTup3 a b d) -> Term c s (PHList '[a, b, d])
  toHList = punsafeCoerce

type family Nth n (xs :: [k]) where
  Nth 0 (x : xs) = x
  Nth n (_ : xs) = Nth (n - 1) xs

type PForgetTerm :: Scope -> PType
data PForgetTerm c s = forall a.
  MkPForgetTerm {recallTerm :: Term c s a}

type family (xs :: [k]) :++: (ys :: [k]) :: [k] where
  '[] :++: ys = ys
  (x ': xs) :++: ys = x ': (xs :++: ys)

pconcat :: Term Expr s (PHList xs) -> Term Expr s (PHList ys) -> Term c s (PHList (xs :++: ys))
pconcat = declareOperator "+"

pnil :: Term c s (PHList '[])
pnil = MkTerm $ \_ -> ListLit []

pcons :: Term Expr s x -> Term Expr s (PHList xs) -> Term c s (PHList (x : xs))
pcons = pconcat . psingleton

(#:) :: Term 'Expr s x -> Term 'Expr s (PHList xs) -> Term c s (PHList (x : xs))
x #: xs = pcons x xs

infixr 5 #:

var :: Int -> SQF
var = LocalVar . mkVar

term :: SQF -> Term c s a
term = MkTerm . const

exampleList :: Term Expr s (PHList '[PInteger, PBool, PString])
exampleList = pconstant 4 #: pcon PTrue #: pconstant "214" #: pnil