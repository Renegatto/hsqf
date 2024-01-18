{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Examples
  ( someResult,
    someLambda,
    someProcedure,
    thisPlayer,
    infAmmoForEveryUnitOf,
  )
where

import HSQF.Api
  ( PEvent (PFired),
    PPlayer,
    addEventHandler,
    currentvehiclespeed,
    forEach,
    punsafeDowncast,
    setvehicleammo,
    units,
    PUnit
  )
import HSQF.Language.Monadic qualified as P
import HSQF.Prelude
import HSQF.Language.Record


type LAMBSState :: [RecordField]
type LAMBSState =
   '[ "dangerLevel" ':= PInteger
    , "autoAdjustment" ':= PBool
    , "targets" ':= PList PUnit 
    ]

type PLAMBSState :: PType
newtype PLAMBSState s = MkPLambsState
  (Term 'Expr s (PRecord LAMBSState))
  deriving IsPRecord via PNewtype (PRecord LAMBSState)

type RecExample = '["someField" ':= PInteger, "someOtherField" ':= PBool]

q :: Term 'Expr s (PRecord '["someField" ':= PInteger, "someOtherField" ':= PBool])
  -> Term 'Expr s PInteger
q rc = get @"someField" rc -- getRecordField @"suck" rc

type Example = "sdfs" ':= PBool :: RecordField

state :: forall c s. Term c s PLAMBSState
state = fromRecord $
  pconsRecord (pconstant @Integer 55)
  . pconsRecord (pcon PTrue)
  . pconsRecord (pempty @PUnit)
  $ pemptyRecord


auto :: Term c s PBool
auto = foo state -- [55.0, true, []] select 1;

foo :: Term 'Expr s PLAMBSState -> Term c s PBool
foo x = get @"autoAdjustment" x

someResult :: Term c s PInteger
someResult = someLambda # psingleton (pconstant @Integer 1)

someLambda :: Term c s ('[PInteger] :==> PInteger)
someLambda = pprocedure $ \x -> P.do
  n <- plet (pconstant 12)
  let fstNumber = sel @0 $ n #: n #: pnil
  (fstNumber + x) `currentvehiclespeed` x

someProcedure :: Term c s ('[PInteger] :==> PInteger)
someProcedure = pprocedure $ \x ->
  plet (pconstant 12) $ \n ->
    let fstNumber = sel @0 $ n #: n #: pnil
     in (fstNumber + x) `currentvehiclespeed` x

thisPlayer :: Term 'Expr s PPlayer
thisPlayer = declareGlobal "this"

-- | Units means to be artillery
infAmmoForEveryUnitOf ::
  forall s.
  Term 'Expr s PPlayer ->
  Term 'Stat s PVoid -- All this term is a statement, because of use of `plet`
infAmmoForEveryUnitOf player = P.do
  event <- plet $ pcon PFired
  giveInfAmmo <- plet $
    pprocedure $ \arty -> P.do
      reloadAmmo <- plet $
        ptask $
          pprocedure $ \forVehicle -> P.do
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

compiled :: String
compiled = compile $ infAmmoForEveryUnitOf thisPlayer

{-
>>> compiled
"private _var0 = \"fired\"; private _var1 = {  (params [\"_var2\"]);  private _var3 = {    (params [\"_var4\"]);    (_var4 setvehicleammo 1.0); };   private _var4 = _var2;   (_var4 addEventHandler ([_var0] + ([_var3] + []))); }; (_var1 forEach (units this));"

-}
