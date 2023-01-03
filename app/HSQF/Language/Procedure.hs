{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module HSQF.Language.Procedure (pswitch, pprocedure, plazy) where

import Data.Kind (Constraint)
import HSQF.Language.Common
  ( PBool,
    PInteger,
    PString,
    PType,
    S,
    Scope (Expr),
    Term,
    mkVar,
    type (:==>),
    var,
    term,
    PEq,
  )
import HSQF.Language.Definition (Term (MkTerm, runTerm))
import SQF
  ( SQF
      ( ListLit,
        Procedure,
        StringLit,
        UnaryOperator,
        Switch
      ),
  )
import Control.Arrow (Arrow((***)))

pprocedure ::
  forall (ret :: PType) (c :: Scope) (c' :: Scope) (args :: [PType]) (s :: S).
  MatchArgs args ret c s =>
  Next args ret c s ->
  Term c' s (args :==> ret)
pprocedure f = MkTerm $ \lvl -> nextArg @args @ret @c @s lvl [] f

plazy :: Term c s a -> Term c s ('[] :==> a)
plazy x = MkTerm $ \lvl -> Procedure [runTerm x lvl]

pswitch :: forall a b c s.
  PEq a =>
  Term 'Expr s a ->
  [(Term 'Expr s a, Term c s ('[] :==> b))] ->
  (Maybe (Term c s ('[] :==> b))) ->
  Term c s b
pswitch on cases whenNothingMatched = MkTerm $ \lvl ->
  Switch
    (runTerm on lvl)
    ((flip runTerm lvl *** flip runTerm lvl) <$> cases)
    (flip runTerm lvl <$> whenNothingMatched) 

type MatchArgs :: [PType] -> PType -> Scope -> S -> Constraint
class MatchArgs xs b c s where
  type Next xs b c s = r | r -> xs b c s
  nextArg :: Int -> [String] -> Next xs b c s -> SQF

instance MatchArgs xs b c s => MatchArgs (x : xs) b c s where
  type Next (x : xs) b c s = (Term 'Expr s x -> Next xs b c s)
  nextArg :: Int -> [String] -> (Term 'Expr s x -> Next xs b c s) -> SQF
  nextArg lvl vars f =
    nextArg @xs @b @c @s
      (succ lvl)
      (mkVar lvl : vars)
      (f $ term $ var lvl)

instance MatchArgs '[] b c s where
  type Next '[] b c s = Term c s b
  nextArg :: Int -> [String] -> Term c s b -> SQF
  nextArg lvl vars result =
    Procedure
      [ UnaryOperator "params" $ ListLit (StringLit . mappend "_" <$> vars),
        runTerm result lvl
      ]

inferenceExample = pprocedure @PInteger @_ @_ @'[PInteger, PString, PBool] $ \a b c ->
  a -- inferred

reverseInferenceExample :: _
reverseInferenceExample = pprocedure @PBool {- args type inferred -} $
  \(_ :: Term 'Expr x PInteger) (b :: Term 'Expr x PBool) (_ :: Term 'Expr x PString) ->
    b
