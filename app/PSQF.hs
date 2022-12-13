{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
import PSQF.Definition
import PSQF.HList
import USQF (SQF(GlobalVar))

currentvehiclespeed :: Term c s ('[PInteger,PInteger] :==> PInteger)
currentvehiclespeed =
  MkTerm $ \_ -> GlobalVar "currentvehiclespeed"

gc = gCorrect # psingleton (pconstant @Integer 1)

gCorrect :: Term Stat s ('[PInteger] :==> PInteger)
gCorrect = plam @'[PInteger] $ \(MkFlip x) ->
  plet (pconstant @Integer 12) $ \n -> 
    let q = pcon $ MkPPair n n
        q0 = sel @0 $ toHList q
    in currentvehiclespeed # toHList (pcon $ MkPPair (q0 + x) x)


-- g :: Term Stat s ('[PInteger] :==> PInteger)
-- g = plam @'[PInteger] $ \(MkFlip x) ->
--   let q0 = sel @0 $ toHList q
--   in currentvehiclespeed # toHList (pcon $ MkPPair (q0 + x) x)

q :: Term Stat s (PPair PInteger PInteger)
q = plet (pconstant @Integer 12) $ \n -> pcon $ MkPPair n n

{-

THE expr 'LET' is impossible, BOY :(

>>> runTerm gc 0
{ params call [_var0];
  private _var1 = 12;
  currentvehiclespeed call [(select call [0,[_var1,_var1]]) + _var0,_var0];
} call [1];


{ params call [_var0];
  currentvehiclespeed call
    [ select call
        [0, let private _var1 = 12.0 in [_var1,_var1]]) + _var0 
    ,_var0
    ];
} call [1];



{ params call [var1];
  currentvehiclespeed call
    [(select call [0, let private var6 = 12 in [var6,var6]]) + var1
    ,var1
    ]
  ]
} call [1]

>>> q 0
Couldn't match expected type: t0 -> t
Couldn't match expected type: t0 -> t
            with actual type: Term 'Stat s0 (PPair PInteger PInteger)
-}

  -- Constant :: forall (a :: Type) s. Lift a => a -> SQF s (Lifted a)
  -- Downcast :: forall b s a. Subtype a b => SQF s a -> SQF s b 
  -- SCon :: forall s (a :: SType). a s -> SQF s a
  -- BuiltinFn :: forall s a b. String -> SQF s (a :--> b)
  -- Procedure :: forall a b s. (forall s. SQF s a -> SQF s b) -> SQF s (a :--> b)
  -- Call :: forall a b s. (forall s0. SQF s0 (a :--> b)) -> SQF s a -> SQF s b  


