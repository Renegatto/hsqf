{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
module HSQF where
import Data.Kind (Type)
import Data.List (intercalate)
import qualified SQF
import SQF (Statement(BindLocally))

data S
type SType = S -> Type

data STuple (a :: SType) (b :: SType) (s :: S) =
  SQF s a ::: SQF s b
data SString (s :: S) = MkSSTring
data SObject (s :: S) = MkObject
data SVoid (s :: S) = MkVoid
newtype SVehicle (s :: S) = MkSVehicle (SObject s)
data SInteger (s :: S) = MkSInteger

-- | SQF Procedure
type (:-->) :: SType -> SType -> SType
data (:-->) a b s = MkArrow  

class Lift (a :: Type) where
  type Lifted a :: SType

instance Lift [Char] where type Lifted String = SString
instance Lift Integer where type Lifted Integer = SInteger

data SQF (s :: S) (a :: SType) where
  Let :: SQF s a -> (SQF s a -> SQF s b) -> SQF s b
  Constant :: forall (a :: Type) s. Lift a => a -> SQF s (Lifted a)
  Downcast :: forall b s a. Subtype a b => SQF s a -> SQF s b 
  SCon :: forall s (a :: SType). a s -> SQF s a
  BuiltinFn :: forall s a b. String -> SQF s (a :--> b)
  Procedure :: forall a b s. (forall s. SQF s a -> SQF s b) -> SQF s (a :--> b)
  Call :: forall a b s. (forall s0. SQF s0 (a :--> b)) -> SQF s a -> SQF s b  

mkVar :: Int -> String
mkVar = mappend "var" . show

compileExpr :: Int -> SQF s a -> Maybe SQF.Expression
compileExpr = undefined

j :: forall (s :: S). SQF s (SInteger ::: SInteger)
j = Let (BuiltinFn "foo" # Constant @Integer 3) $ \x -> spair x x

-- compile :: Int -> SQF s a -> Maybe SQF.Statement
-- compile argN = \case
--   Let def scope -> do
--     compiledDef <- compileExpr (succ argN) def
--     continuation <- compile (succ argN) (scope )
--     let binding = BindLocally (mkVar argN) compiledDef
--     pure $ binding SQF.#> continuation

--   Constant a -> _
--   Downcast sqf -> _
--   SCon as -> _
--   BuiltinFn s -> _
--   Procedure x0 -> _
--   Call x0 sqf -> _

(#) :: forall {s :: S} {a :: SType} {b :: SType}.
  (forall s'. SQF s' (a :--> b)) -> SQF s a -> SQF s b
f # x = Call f x

scon = SCon

spair :: SQF s a -> SQF s b -> SQF s (a ::: b)
spair a b = scon (a ::: b)

type (:::) :: SType -> SType -> SType
type a ::: b  = STuple a b
infixr 6 :::
infixr 1 :-->


--this addEventHandler ["fired", { (_this # 0) setvehicleammo 1} ]; 
class Subtype (a :: SType) (b :: SType)
instance Subtype SObject SVehicle

term :: forall {s :: S}. SQF s (SObject :--> SVoid)
term = Procedure $ \this ->
  addEventHandler #
    ( let event = Constant "fired"
          handler :: forall s0. SQF s0 (SObject :--> SVoid)
          handler = Procedure $ \(triggeredObj :: SQF s1 SObject) ->
            setvehicleammo
              # spair
                (Downcast @SVehicle @s1 triggeredObj)
                (Constant @Integer 1)
      in spair this (spair event handler)
    )

sfst :: forall s a b. SQF s (a ::: b :--> a) 
sfst = BuiltinFn @s @(a ::: b) @a "#"

addEventHandler :: forall s b.
  SQF s (SObject ::: SString ::: (SObject :--> b) :--> SVoid) 
addEventHandler =
  BuiltinFn @s @(SObject ::: SString ::: (SObject :--> b)) @SVoid 
    "addEventHandler"

setvehicleammo :: forall s. SQF s (SVehicle ::: SInteger :--> SVoid) 
setvehicleammo =
  BuiltinFn @s @(SVehicle ::: SInteger) @SVoid "setvehicleammo"