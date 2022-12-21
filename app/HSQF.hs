{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module HSQF where

import HSQF.Language.Common
  ( PCon (pcon),
    PConstant (pconstant),
    PInteger,
    PVoid,
    Scope (Expr, Stat),
    Term,
    declareGlobal,
    type (:==>),
  )
import HSQF.Language.HList
  ( Flip (MkFlip),
    plam,
    plet,
    pnil,
    psingleton,
    sel,
    (#),
    (#:),
  )
import SQF (SQF (GlobalVar), compile, unNewLine)
import HSQF.Api
  ( PEvent (PFired),
    PPlayer,
    addEventHandler,
    currentvehiclespeed,
    forEach,
    punsafeDowncast,
    setvehicleammo,
    units,
  )
import qualified HSQF.Language.Monadic as P
import HSQF.Language.Procedure (pprocedure)
import HSQF.Language.Subtyping (pcontraFirst)
import HSQF.Language.Task (ptask)

someResult :: Term c s PInteger
someResult = someLambda # psingleton (pconstant @Integer 1)

someLambda:: Term c s ('[PInteger] :==> PInteger)
someLambda = plam $ \(MkFlip x) ->
  plet (pconstant 12) $ \n -> 
    let fstNumber = sel @0 $ n #: n #: pnil
    in (fstNumber + x) `currentvehiclespeed` x

someProcedure:: Term c s ('[PInteger] :==> PInteger)
someProcedure = pprocedure $ \x ->
  plet (pconstant 12) $ \n -> 
    let fstNumber = sel @0 $ n #: n #: pnil
    in (fstNumber + x) `currentvehiclespeed` x

thisPlayer :: Term 'Expr s PPlayer
thisPlayer = declareGlobal "this"

-- | Units means to be artillery
infAmmoForEveryUnitOf ::
  forall c s.
  Term Expr s PPlayer ->
  Term Stat s PVoid -- All this term is a statement, because of use of `plet`
infAmmoForEveryUnitOf player = P.do
  event <- plet $ pcon PFired
  giveInfAmmo <- plet $ pprocedure $ \arty -> P.do
    reloadAmmo <- plet $ ptask $ pprocedure $ \forVehicle -> P.do
      {- Note, that we can't use `event` from outer scope here,
        because `reloadAmmo` is a `PTask` (asyncronous CODE)
        and therefore have only *own* and *global* scopes.
        `PTask` is not a closure.

        We achieve this property to typecheck simply
        by universally quantifying over `s` (context type variable).
      -} 
      -- _ <- plet $ event
      forVehicle `setvehicleammo` pconstant @Integer 1
    artyAsVehicle <- plet $ punsafeDowncast arty
    {- Note that we can use `event` from outer scope here,
      because procedure (CODE) is simply a closure
    -}
    artyAsVehicle `addEventHandler` (event #: reloadAmmo #: pnil)
  giveInfAmmo `forEach` units player

{-
>>> unNewLine $ compile 0 $ runTerm (reloadAmmoForEveryUnitOf player) 0
"private _var0 = \"fired\"; private _var1 = {  (params [\"_var2\"]);  private _var3 = {    (params [\"_var4\"]);    (_var4 setvehicleammo 1.0); };   private _var4 = _var2;   (_var4 addEventHandler ([_var0] + ([_var3] + []))); }; (_var1 forEach (units this))"
-}