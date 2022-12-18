{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
module PSQF.Api where
import PSQF.Definition 
import PSQF.HList (pnil)
import USQF (SQF(ListLit))


newtype PObject s = MkPObject { getObject :: Term Expr s PObject }
newtype PUnit s = MkPUnit { getUnit :: Term Expr s PUnit }
newtype PPlayer s = MkPPlayer { getPlayer :: Term Expr s PPlayer }

class PSubtype a b where
  pupcast :: Term Expr s a -> Term c s b
instance PSubtype PUnit PObject where
  pupcast = punsafeCoerce

newtype PList (a :: PType) s = MkPList {getPList :: Term Expr s (PList a)}

-- pappend :: Term Expr s (PList a) -> Term Expr s a -> Term c s (PList a)
-- pappend = declareOperator "append"

pempty :: Term c s (PList a)
pempty = MkTerm $ \_ -> ListLit [] 

units :: Term c s (PPlayer :--> PList PUnit)
units = declareGlobal "units"