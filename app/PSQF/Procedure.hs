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

import Data.Kind (Constraint)
import PSQF.Common
  ( PBool,
    PInteger,
    PString,
    PType,
    S,
    Scope (Expr),
    Term (..),
    mkVar,
    type (:==>),
  )
import PSQF.Definition (Term (MkTerm, runTerm))
import PSQF.HList (term, var)
import SQF
  ( SQF
      ( Call,
        GlobalVar,
        ListLit,
        LocalVar,
        Procedure,
        StringLit,
        UnaryOperator
      ),
  )

pprocedure ::
  forall (ret :: PType) (c :: Scope) (c' :: Scope) (args :: [PType]) (s :: S).
  MatchArgs args ret c s =>
  Next args ret c s ->
  Term c' s (args :==> ret)
pprocedure f = MkTerm $ \lvl -> nextArg @args @ret @c @s lvl [] f

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
      [ UnaryOperator "params" $ ListLit (StringLit . mappend "_" <$> vars)
      , runTerm result lvl
      ]

inferenceExample = pprocedure @PInteger @_ @_ @'[PInteger,PString,PBool] $ \a b c ->
  a -- inferred

reverseInferenceExample :: _
reverseInferenceExample = pprocedure @PBool {- args type inferred -} $
  \(a :: Term Expr x PInteger) (b :: Term Expr x PBool) (c :: Term Expr x PString) ->
    b
