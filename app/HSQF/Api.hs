{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module HSQF.Api where

import HSQF.Language.Common
  ( PCon (..),
    PInteger,
    PType,
    PVoid,
    Scope (Expr),
    Term,
    declareOperator,
    declareUnary,
    punsafeCoerce,
    type (:==>),
  )
import HSQF.Language.HList (PHList, pnil)
import SQF (SQF (GlobalVar, ListLit, StringLit))
import HSQF.Language.Task (PTask)
import HSQF.Language.Definition (Term(MkTerm))

newtype PObject s = MkPObject { getObject :: Term Expr s PObject }
newtype PUnit s = MkPUnit { getUnit :: Term Expr s PUnit }
newtype PPlayer s = MkPPlayer { getPlayer :: Term Expr s PPlayer }
newtype PVehicle s = MkPVehicle { getVehicle :: Term Expr s PVehicle }

type PEvent :: PType
data PEvent s =
  PFired | PHealed

instance PCon PEvent where
  pcon PFired = MkTerm $ \_ -> StringLit "fired"
  pcon PHealed = MkTerm $ \_ -> StringLit "healed"

-- | PSubtype a b means that a is subtype of b
class PSubtype a b where
  pupcast :: Term Expr s a -> Term c s b
instance PSubtype PUnit PObject where
  pupcast = punsafeCoerce
instance PSubtype PVehicle PUnit where
  pupcast = punsafeCoerce
instance PSubtype PVehicle PObject where
  pupcast = punsafeCoerce

punsafeDowncast :: PSubtype a b => Term Expr s b -> Term c s a
punsafeDowncast = punsafeCoerce

newtype PList (a :: PType) s = MkPList
  {getPList :: Term Expr s (PList a)}

pempty :: Term c s (PList a)
pempty = MkTerm $ \_ -> ListLit [] 

units :: Term Expr s PPlayer -> Term c s (PList PUnit)
units = declareUnary "units"

currentvehiclespeed ::
  Term Expr s PInteger ->
  Term Expr s PInteger ->
  Term c s PInteger
currentvehiclespeed = declareOperator "currentvehiclespeed"

-- | Applies CODE to every element of array and returns last result
forEach ::
  Term Expr s ('[a] :==> b) ->
  Term Expr s (PList a) ->
  Term c s b
forEach = declareOperator "forEach"

setvehicleammo ::
  Term Expr s PVehicle ->
  Term Expr s PInteger ->
  Term c s PVoid
setvehicleammo = declareOperator "setvehicleammo"

addEventHandler :: forall a c s. PSubtype a PObject =>
  Term Expr s a ->
  Term Expr s (PHList '[PEvent, PTask ('[a] :==> PVoid)]) ->
  Term c s PVoid
addEventHandler = declareOperator "addEventHandler"
