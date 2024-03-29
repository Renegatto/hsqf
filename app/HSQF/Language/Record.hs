{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveAnyClass #-}
module HSQF.Language.Record
  ( PRecord
  , RecordField (type (:=))
  , IsPRecord (fromRecord, toRecord)
  , pconsRecord
  , pemptyRecord
  , get
  , record
  , unRecord
  ) where
import HSQF.Prelude
import GHC.TypeLits (Symbol, Nat, type (+), TypeError, KnownNat, ErrorMessage (Text))
import HSQF.Language.HList (Nth)
import qualified HSQF.Language.Monadic as P
import Data.Kind (Type)
import qualified GHC.Generics as GHC
import Generics.SOP (Generic)

type RecordField :: Type
data RecordField = Symbol := PType

type RecFields :: [PType] -> [RecordField] 
type family RecFields fields = r | r -> fields where
  RecFields '[] = '[]
  RecFields (LabeledTerm label pt : fields) =
    (label ':= pt : RecFields fields)

type UnRecordFields :: [RecordField] -> [PType]
type family UnRecordFields fields = r | r -> fields where
  UnRecordFields '[] = '[]
  UnRecordFields (label ':= pt : fields) =
    (LabeledTerm label pt : UnRecordFields fields)

type LabeledTerm :: Symbol -> PType -> PType
data LabeledTerm label a s = MkLabeledTerm
  { runLabeledTerm :: Term 'Expr s a }

instance PCon (LabeledTerm label a) where
  type PConstructed (LabeledTerm label a) = (LabeledTerm label a)
  pcon = punsafeCoerce @a @(LabeledTerm label a) . runLabeledTerm 
instance PMatch (LabeledTerm label a) where
  type PPattern (LabeledTerm label a) = (LabeledTerm label a)
  match x f = f $ MkLabeledTerm $
    punsafeCoerce @(LabeledTerm label a) @a x

type PRecord :: [RecordField] -> PType
newtype PRecord fields s = MkPRecord
  (Term 'Expr s (PHList (UnRecordFields fields)))
  deriving stock GHC.Generic
  deriving anyclass Generic

plabelTerm ::
  forall label a c s.
  Term 'Expr s a ->
  Term c s (LabeledTerm label a)
plabelTerm = pcon . MkLabeledTerm

-- * Building a PRecord

pconsRecord ::
  forall label a fields c s.
  Term 'Expr s a ->
  Term 'Expr s (PRecord fields) ->
  Term c s (PRecord (label ':= a : fields))
pconsRecord a = record . (plabelTerm a #:) . unRecord

pemptyRecord :: Term c s (PRecord '[])
pemptyRecord = record pnil

class IsPRecord (a :: PType) where
  type RecordOf a :: [RecordField] -- | c -> a 
  toRecord :: Term 'Expr s a -> Term c s (PRecord (RecordOf a)) 
  fromRecord :: Term 'Expr s (PRecord (RecordOf a)) -> Term c s a 

instance IsPRecord (PRecord xs) where
  type RecordOf (PRecord xs) = xs
  toRecord = unExpr
  fromRecord = unExpr
  
instance IsPRecord (PNewtype (PRecord xs)) where
  type RecordOf (PNewtype (PRecord xs)) = xs
  toRecord = unExpr . pfromNewtype
  fromRecord = unExpr . pnewtype

type GetRecordFieldTypeG label =
  forall fields a s c xs n r.
  ( n ~ IndexOf label xs 0
  , IsPRecord r
  , RecordOf r ~ fields
  , KnownNat n
  , UnRecordFields fields ~ xs
  , Nth n xs ~ (LabeledTerm label a)) =>
  Term 'Expr s r ->
  Term c s a

get :: forall label. GetRecordFieldTypeG label
get = getLabeledElem @label . unRecord . toRecord

{- * Coercion between PHList and PRecord
 those are represintationally the same, so we just `punsafeCoerce`
-}

record ::
  forall fields c s.
  Term 'Expr s (PHList (UnRecordFields fields)) ->
  Term c s (PRecord fields)
record =
  punsafeCoerce @(PHList (UnRecordFields fields)) @(PRecord fields) 

unRecord ::
  forall fields c s.
  Term 'Expr s (PRecord fields) ->
  Term c s (PHList (UnRecordFields fields))
unRecord =
  punsafeCoerce @(PRecord fields) @(PHList (UnRecordFields fields))

getLabeledElem ::
  forall label (xs :: [PType]) a s c n.
  ( n ~ IndexOf label xs 0
  , KnownNat n
  , Nth n xs ~ (LabeledTerm label a)) =>
  Term Expr s (PHList xs) ->
  Term c s a
getLabeledElem xs = P.do 
  MkLabeledTerm t <- match $ sel @n xs
  unExpr t

type IndexOf :: Symbol -> [a] -> Nat -> Nat
type family IndexOf label xs n = n' where
  IndexOf label (LabeledTerm label a : xs) n = n
  IndexOf label (LabeledTerm _ _ : xs) n = IndexOf label xs (n + 1)
  IndexOf _ _ _ = TypeError ('Text "There is no such label")
