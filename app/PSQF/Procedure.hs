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
  ) =>
  (forall x. Next args ret x) -> Term c s (args :==> ret)
pprocedure f = MkTerm impl
  where
    impl :: forall (s' :: S). Int -> SQF
    impl lvl =
      nextArg @args @ret @s' lvl [] f

type MatchArgs :: [PType] -> PType -> S -> Constraint
class MatchArgs xs b s where
  type Next xs b s = c | c -> xs b s
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

inferenceExample = pprocedure @PInteger @'[PInteger,PString,PBool] $ \a b c ->
  a -- inferred

reverseInferenceExample :: _
reverseInferenceExample = pprocedure @PBool {- args type inferred -} $
  \(a :: Term Expr x PInteger) (b :: Term Expr x PBool) (c :: Term Expr x PString) -> b
