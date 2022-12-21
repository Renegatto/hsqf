module HSQF.Language.Subtyping
  ( PSubtype (pupcast),
    pcontraFirst,
    pcontra,
  )
where

import HSQF.Language.Common (punsafeCoerce)
import HSQF.Language.Definition (Scope (Expr), Term, type (:==>))

step ::
  (Term c s (xs :==> b) -> Term c s (ys :==> b')) ->
  Term c s ((x : xs) :==> b) ->
  Term c s ((x : ys) :==> b)
step f xs = punsafeCoerce $ f $ punsafeCoerce xs

pcontraFirst ::
  PSubtype sub super =>
  Term c s ((super : xs) :==> a) -> -- wider
  Term c s ((sub : xs) :==> a) -- smaller
pcontraFirst = punsafeCoerce

-- | PSubtype a b means that a is subtype of b
class PSubtype a b where
  pupcast :: Term 'Expr s a -> Term c s b

class SubFunction ys xs where
  pcontra :: Term c s (xs :==> a) -> Term c s (ys :==> a)

instance SubFunction '[] '[] where
  pcontra :: Term c s ('[] :==> a) -> Term c s ('[] :==> a)
  pcontra = id

instance (SubFunction ys xs, PSubtype y x) => SubFunction (y : ys) (x : xs) where
  pcontra :: Term c s ((x : xs) :==> a) -> Term c s ((y : ys) :==> a)
  pcontra f = step pcontra $ pcontraFirst @y f