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
module PSQF where
import PSQF.Definition
import PSQF.HList
import USQF (SQF(GlobalVar),compile,unNewLine)
import qualified PSQF.Monadic as P
import PSQF.Api
import PSQF.Procedure (pprocedure)
import PSQF.Subtyping (pcontraFirst)
import PSQF.Task (ptask)

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
  Term Stat s PVoid
infAmmoForEveryUnitOf player = P.do
  event <- plet $ pcon PFired
  giveInfAmmo <- plet $ pprocedure $ \arty -> P.do
    reloadAmmo <- plet $ ptask $ pprocedure $ \forVehicle ->
      forVehicle `setvehicleammo` pconstant @Integer 1
    artyAsVehicle <- plet $ punsafeDowncast arty
    artyAsVehicle `addEventHandler` (event #: reloadAmmo #: pnil)
  giveInfAmmo `forEach` units player

{-
>>> unNewLine $ compile 0 $ runTerm (reloadAmmoForEveryUnitOf player) 0
"private _var0 = \"fired\"; private _var1 = {  (params [\"_var2\"]);  private _var3 = {    (params [\"_var4\"]);    (_var4 setvehicleammo 1.0); };   private _var4 = _var2;   (_var4 addEventHandler ([_var0] + ([_var3] + []))); }; (_var1 forEach (units this))"
-}