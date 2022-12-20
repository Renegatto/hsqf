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
  forall (ret :: PType) (c :: Scope) (c' :: Scope) (args :: [PType]) (s :: S).
  ( MatchArgs args ret c s
  , UnLamHasNext args -- Needs to avoid GHC bug related to type inference (captured at f0507fcd60)
  ) =>
  (forall x. Next args ret c x) -> Term c' s (args :==> ret)
pprocedure f = MkTerm $ \lvl -> nextArg @args @ret @c @s lvl [] f

type UnLambdaOf :: [PType] -> PType -> Scope -> PType
type family UnLambdaOf args b c s = f | f -> args b c s where
  UnLambdaOf '[] b c s = Term c s b
  UnLambdaOf (x:xs) b c s = Term Expr s x -> UnLambdaOf xs b c s

-- | Just a proof for GHC
class UnLamHasNext xs where
  unLamHasNext :: forall b c s. Equal (UnLambdaOf xs b c s) (Next xs b c s)
instance UnLamHasNext '[] where
  unLamHasNext = EqRefl
instance UnLamHasNext as => UnLamHasNext (a:as) where
  unLamHasNext :: forall b c s. Equal (UnLambdaOf (a : as) b c s) (Next (a : as) b c s)
  unLamHasNext = liftEq $ unLamHasNext @as @b @c @s

data Equal a b where
  EqRefl :: forall a. Equal a a 

liftEq :: forall f a b. Equal a b -> Equal (f a) (f b)
liftEq EqRefl = EqRefl

eqCoerce :: Equal a b -> a -> b
eqCoerce EqRefl x = x

type MatchArgs :: [PType] -> PType -> Scope -> S -> Constraint
class MatchArgs xs b c s where
  type Next xs b c s = r | r -> xs b c s
  nextArg :: Int -> [String] -> Next xs b c s -> SQF

instance MatchArgs xs b c s => MatchArgs (x:xs) b c s where
  type Next (x:xs) b c s = (Term Expr s x -> Next xs b c s)
  nextArg :: Int -> [String] -> (Term Expr s x -> Next xs b c s) -> SQF
  nextArg lvl vars f =
    nextArg @xs @b @c @s (succ lvl) (mkVar lvl : vars) (f $ term $ var lvl)

instance MatchArgs '[] b c s where
  type Next '[] b c s = Term c s b
  nextArg :: Int -> [String] -> Term c s b -> SQF
  nextArg lvl vars result =
    Procedure
      [ Call (GlobalVar "params") $ ListLit (StringLit <$> vars)
      , runTerm result lvl
      ]

inferenceExample = pprocedure @PInteger @_ @_ @'[PInteger,PString,PBool] $ \a b c ->
  a -- inferred

reverseInferenceExample :: _
reverseInferenceExample = pprocedure @PBool {- args type inferred -} $
  \(a :: Term Expr x PInteger) (b :: Term Expr x PBool) (c :: Term Expr x PString) -> b
