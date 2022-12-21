{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module HSQF.Language.List (
    PList,
    pcons,
    (#::),
    pempty,
    pconcat,
    psingleton,
  )
  where
import HSQF.Language.Definition (PType, Scope(Expr), Term (MkTerm, runTerm))
import SQF (SQF (ListLit))
import HSQF.Language.Common (declareOperator)

newtype PList (a :: PType) s = MkPList
  {getPList :: Term Expr s (PList a)}

(#::) :: Term Expr s a -> Term Expr s (PList a) -> Term c s (PList a)
(#::) = pcons

pconcat ::
  Term Expr s (PList a) ->
  Term Expr s (PList a) ->
  Term c s (PList a)
pconcat = declareOperator "+"

psingleton :: Term Expr s a -> Term c s (PList a)
psingleton x = MkTerm $ \lvl -> ListLit [runTerm x lvl]

pcons ::
  Term Expr s a ->
  Term Expr s (PList a) ->
  Term c s (PList a)
pcons = pconcat . psingleton

pempty :: Term c s (PList a)
pempty = MkTerm $ \_ -> ListLit []