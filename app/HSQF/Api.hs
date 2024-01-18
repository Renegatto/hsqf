{-# LANGUAGE TypeFamilies #-}

module HSQF.Api
  ( PObject,
    PUnit,
    PPlayer,
    PVehicle,
    PEvent (PFired, PHealed),
    addEventHandler,
    currentvehiclespeed,
    forEach,
    punsafeDowncast,
    setvehicleammo,
    units,
  )
where

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
import HSQF.Language.Definition (Term (MkTerm))
import HSQF.Language.HList (PHList)
import HSQF.Language.List (PList)
import HSQF.Language.Subtyping (PSubtype (pupcast))
import HSQF.Language.Task (PTask)
import SQF (SQF (StringLit))

newtype PObject s = MkPObject {getObject :: Term 'Expr s PObject}

newtype PUnit s = MkPUnit {getUnit :: Term 'Expr s PUnit}

newtype PPlayer s = MkPPlayer {getPlayer :: Term 'Expr s PPlayer}

newtype PVehicle s = MkPVehicle {getVehicle :: Term 'Expr s PVehicle}

type PEvent :: PType
data PEvent s
  = PFired
  | PHealed

instance PCon PEvent where
  type PConstructed PEvent = PEvent
  pcon PFired = MkTerm $ \_ -> StringLit "fired"
  pcon PHealed = MkTerm $ \_ -> StringLit "healed"

instance PSubtype PUnit PObject where
  pupcast = punsafeCoerce

instance PSubtype PVehicle PUnit where
  pupcast = punsafeCoerce

instance PSubtype PVehicle PObject where
  pupcast = punsafeCoerce

punsafeDowncast :: PSubtype a b => Term 'Expr s b -> Term c s a
punsafeDowncast = punsafeCoerce

units :: Term 'Expr s PPlayer -> Term c s (PList PUnit)
units = declareUnary "units"

currentvehiclespeed ::
  Term 'Expr s PInteger ->
  Term 'Expr s PInteger ->
  Term c s PInteger
currentvehiclespeed = declareOperator "currentvehiclespeed"

-- | Applies CODE to every element of array and returns last result
forEach ::
  Term 'Expr s ('[a] :==> b) ->
  Term 'Expr s (PList a) ->
  Term c s b
forEach = declareOperator "forEach"

setvehicleammo ::
  Term 'Expr s PVehicle ->
  Term 'Expr s PInteger ->
  Term c s PVoid
setvehicleammo = declareOperator "setvehicleammo"

addEventHandler ::
  forall a c s.
  PSubtype a PObject =>
  Term 'Expr s a ->
  Term 'Expr s (PHList '[PEvent, PTask ('[a] :==> PVoid)]) ->
  Term c s PVoid
addEventHandler = declareOperator "addEventHandler"
