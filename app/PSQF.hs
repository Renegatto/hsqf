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
module PSQF where
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

class PConstant a where
  type PConst a :: PType
  pconstant :: a -> Term s (PConst a)

class PCon (a :: PType) where 
  pcon :: a s -> Term s a

type PPair :: PType -> PType -> PType
data PPair a b s = MkPPair (Term s a) (Term s b)

type PInteger :: PType
newtype PInteger s = MkInteger { runPInteger :: Term s PInteger }

instance PCon PInteger where
  pcon :: PInteger s -> Term s PInteger
  pcon = runPInteger

instance PCon (PPair a b) where
  pcon :: PPair a b s -> Term s (PPair a b)
  pcon (MkPPair a b) = MkTerm $ \lvl ->
    ListLit [runTerm a lvl, runTerm b lvl]

punsafeCoerce :: Term s a -> Term s b
punsafeCoerce = unsafeCoerce

instance PConstant Integer where
  type PConst Integer = PInteger
  pconstant :: Integer -> Term s (PConst Integer)
  pconstant n = MkTerm $ \_ -> NumLit $ fromIntegral n

plet :: Term s a -> (Term s a -> Term s b) -> Term s b
plet def scope = MkTerm $ \lvl ->
  let varid = mkVar lvl
      def' = runTerm def (succ lvl) 
      scope' = runTerm (scope $ MkTerm $ \_ -> LocalVar varid) (succ lvl) 
  in Seq (BindLocally (mkVar lvl) def') scope'

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

type PBool :: PType
data PBool s = PTrue | PFalse

pif :: Term s PBool -> Term s a -> Term s a -> Term s a
pif b success failure = MkTerm $ \lvl ->
  If (runTerm b lvl) (runTerm success lvl) (runTerm failure lvl)

class PMatch (a :: PType) where
  type PPattern a :: PType
  match :: Term s a -> (PPattern a s -> Term s b) -> Term s b

ppairToList :: Term s (PPair a b) -> Term s (PHList '[a,b])
ppairToList = punsafeCoerce


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

(##) :: (forall s0. Term s0 (a :--> b)) -> Term s a -> Term s b
f ## x = MkTerm $ \lvl ->
  Call (runTerm f lvl) (runTerm x lvl)
 
(#) :: (forall s0. Term s0 (args :==> b)) -> Term s (PHList args) -> Term s b
f # x = MkTerm $ \lvl ->
  Call (runTerm f lvl) (runTerm x lvl)

mkVar :: Int -> String
mkVar = mappend "var" . show

binop op a b =
  MkTerm $ \lvl ->
    BinaryOperator
    op
    (runTerm a lvl)
    (runTerm b lvl)

newtype PHList (xs :: [PType]) s = MkHList
  { runHList :: Term s (PHList xs)} 

instance (PConstant a, PConstant b) => PConstant (a,b) where
  type PConst (a,b) = PHList '[PConst a, PConst b]
  pconstant :: (PConstant a, PConstant b) => (a, b) -> Term s (PConst (a, b))
  pconstant (a,b) = ppairList (pconstant a) (pconstant b)

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
  --select @'[a,b] @b # ppairList (pconstant @Integer 2) pair
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

instance Num (Term s PInteger) where
  (+) :: Term s PInteger -> Term s PInteger -> Term s PInteger
  (+) = binop "+"
  (-) :: Term s PInteger -> Term s PInteger -> Term s PInteger
  (-) = binop "-"
  (*) :: Term s PInteger -> Term s PInteger -> Term s PInteger
  (*) = binop "*"
  fromInteger :: Integer -> Term s PInteger
  fromInteger = pconstant

currentvehiclespeed :: Term s ('[PInteger,PInteger] :==> PInteger)
currentvehiclespeed =
  MkTerm $ \_ -> GlobalVar "currentvehiclespeed"

psingleton :: Term s a -> Term s (PHList '[a])
psingleton x = MkTerm $ \lvl ->
  ListLit [runTerm x lvl]

gc = g # psingleton (pconstant @Integer 1)

g :: Term s ('[PInteger] :==> PInteger)
g = plam @'[PInteger] $ \(MkFlip x) ->
  let q0 = sel @0 $ toHList q
  in currentvehiclespeed # toHList (pcon $ MkPPair (q0 + x) x)

q = plet (pconstant @Integer 12) $ \n -> pcon $ MkPPair n n

{-

THE 'LET' is impossible, BOY :(

>>> runTerm gc 0
Call (Procedure [Call (GlobalVar "params") (ListLit [LocalVar "var0"]),Call (GlobalVar "currentvehiclespeed") (ListLit [BinaryOperator "+" (Call (GlobalVar "select") (ListLit [NumLit 0.0,Seq (BindLocally "var1" (NumLit 12.0)) (ListLit [LocalVar "var1",LocalVar "var1"])])) (LocalVar "var0"),LocalVar "var0"])]) (ListLit [NumLit 1.0])

{ params call [_var0];
  currentvehiclespeed call
    [ select call
        [0, let private _var1 = 12.0 in [_var1,_var1]]) + _var0 
    ,_var0
    ];
} call [1];



{ params call [var1];
  currentvehiclespeed call
    [(select call [0, let private var6 = 12 in [var6,var6]]) + var1
    ,var1
    ]
  ]
} call [1]

>>> q 0
Couldn't match expected type: t0 -> t
            with actual type: Term s0 (PPair PInteger PInteger)

-}

  -- Constant :: forall (a :: Type) s. Lift a => a -> SQF s (Lifted a)
  -- Downcast :: forall b s a. Subtype a b => SQF s a -> SQF s b 
  -- SCon :: forall s (a :: SType). a s -> SQF s a
  -- BuiltinFn :: forall s a b. String -> SQF s (a :--> b)
  -- Procedure :: forall a b s. (forall s. SQF s a -> SQF s b) -> SQF s (a :--> b)
  -- Call :: forall a b s. (forall s0. SQF s0 (a :--> b)) -> SQF s a -> SQF s b  

type family Nth n (xs :: [k]) where
  Nth 0 (x:xs) = x
  Nth n (_:xs) = Nth (n - 1) xs 
