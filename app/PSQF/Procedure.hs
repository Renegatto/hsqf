{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module PSQF.Procedure where
import PSQF.Definition
import USQF (SQF(Procedure, Call, GlobalVar, ListLit, LocalVar))
import Data.Kind (Constraint)
import PSQF.HList (term, var)

pprocedure ::
  forall (ret :: PType) (args :: [PType]) (c :: Scope) (s :: S).
  ( forall x. MatchArgs args ret x
  , UnLamHasNext args
  ) =>
  (forall x. UnLambdaOf args ret Expr x) -> Term c s (args :==> ret)
pprocedure f = MkTerm impl
  where
    impl :: forall (s' :: S). Int -> SQF
    impl lvl =
      nextArg @args @ret @s' lvl []
        ( let proof = unLamHasNext @args @ret @s'
          in eqCoerce proof f :: Next args ret s')

type ResultOf :: [PType] -> PType
type family ResultOf args where
  ResultOf '[a] = a
  ResultOf (_:xs) = ResultOf xs

type UnLambdaOf :: [PType] -> PType -> Scope -> PType
type family UnLambdaOf args b c s = f | f -> args b c s where
  UnLambdaOf '[] b c s = Term c s b
  UnLambdaOf (x:xs) b c s = Term Expr s x -> UnLambdaOf xs b c s

-- | Just a proof for GHC
class UnLamHasNext xs where
  unLamHasNext :: forall b s. Equal (UnLambdaOf xs b Expr s) (Next xs b s)
instance UnLamHasNext '[] where
  unLamHasNext = EqRefl
instance UnLamHasNext as => UnLamHasNext (a:as) where
  unLamHasNext :: forall b s. Equal (UnLambdaOf (a : as) b 'Expr s) (Next (a : as) b s)
  unLamHasNext = liftEq $ unLamHasNext @as @b @s

data Equal a b where
  EqRefl :: forall a. Equal a a 

liftEq :: forall f a b. Equal a b -> Equal (f a) (f b)
liftEq EqRefl = EqRefl

eqCoerce :: Equal a b -> a -> b
eqCoerce EqRefl x = x

type MatchArgs :: [PType] -> PType -> S -> Constraint
class MatchArgs xs b s where
  type Next xs b s
  nextArg :: Int -> [String] -> Next xs b s -> SQF

instance MatchArgs xs b s => MatchArgs (x:xs) b s where
  type Next (x:xs) b s = (Term Expr s x -> Next xs b s)
  nextArg :: Int -> [String] -> (Term Expr s x -> Next xs b s) -> SQF
  nextArg lvl vars f =
    nextArg @xs @b @s (succ lvl) (mkVar lvl : vars) (f $ term $ var lvl)

instance MatchArgs '[] b s where
  type Next '[] b s = Term Expr s b
  nextArg :: Int -> [String] -> Term Expr s b -> SQF
  nextArg lvl vars result =
    Procedure
      [ Call (GlobalVar "params") $ ListLit (LocalVar <$> vars)
      , runTerm result lvl
      ]

u = pprocedure @PInteger @'[PInteger,PString,PBool] $ \a b c -> a

u' :: _
u' = pprocedure @PBool $
  \(a :: Term Expr x PInteger) (b :: Term Expr x PBool) (c :: Term Expr x PString) -> b
