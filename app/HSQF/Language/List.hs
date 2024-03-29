module HSQF.Language.List
  ( PList,
    pcons,
    (#::),
    pempty,
    pconcat,
    psingleton,
  pselect)
where

import HSQF.Language.Common (declareOperator, PInteger)
import HSQF.Language.Definition (PType, Scope (Expr), Term (MkTerm, runTerm))
import SQF (SQF (ListLit))

newtype PList (a :: PType) s = MkPList
  {getPList :: Term 'Expr s (PList a)}

(#::) :: Term 'Expr s a -> Term 'Expr s (PList a) -> Term c s (PList a)
(#::) = pcons

pselect :: Term 'Expr s (PList a) -> Term 'Expr s PInteger -> Term c s a
pselect = declareOperator "#"

pconcat ::
  Term 'Expr s (PList a) ->
  Term 'Expr s (PList a) ->
  Term c s (PList a)
pconcat = declareOperator "+"

psingleton :: Term 'Expr s a -> Term c s (PList a)
psingleton x = MkTerm $ \lvl -> ListLit [runTerm x lvl]

pcons ::
  Term 'Expr s a ->
  Term 'Expr s (PList a) ->
  Term c s (PList a)
pcons = pconcat . psingleton

pempty :: forall a c s. Term c s (PList a)
pempty = MkTerm $ \_ -> ListLit []