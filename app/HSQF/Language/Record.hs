{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLabels #-}
module HSQF.Language.Record where
import HSQF.Prelude
import GHC.TypeLits (Symbol, Nat, type (+), TypeError, KnownNat, ErrorMessage (Text))
import HSQF.Language.HList (Nth)
import qualified HSQF.Language.Monadic as P
import Data.Kind (Type)
import HSQF.Api (PUnit)
import GHC.Records (HasField)
import GHC.OverloadedLabels (IsLabel (fromLabel))
import Data.Proxy (Proxy(Proxy))

type RecordField :: Type
data RecordField = Symbol := PType

-- >>> :k! "asd" := Bool
-- unknown command 'k!'
type Example = "sdfs" ':= PBool :: RecordField

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
  pcon = punsafeCoerce @a @(LabeledTerm label a) . runLabeledTerm 
instance PMatch (LabeledTerm label a) where
  type PPattern (LabeledTerm label a) = (LabeledTerm label a)
  match x f = f $ MkLabeledTerm $
    punsafeCoerce @(LabeledTerm label a) @a x

type PRecord :: [RecordField] -> PType
newtype PRecord fields s = MkPRecord
  { runPRecord :: Term 'Expr s (PHList (UnRecordFields fields)) }

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

class IsPRecord (a :: PType) where
  type RecordOf a :: [RecordField] -- | c -> a 
  toRecord :: Term 'Expr s a -> Term c s (PRecord (RecordOf a)) 
  fromRecord :: Term 'Expr s (PRecord (RecordOf a)) -> Term c s a 
newtype PNewtype a s = MkPNewtype (Term 'Expr s a)

pfromNewtype :: Term c s (PNewtype a) -> Term c s a
pfromNewtype = punsafeCoerce
pnewtype :: Term c s a -> Term c s (PNewtype a)
pnewtype = punsafeCoerce


instance IsPRecord (PRecord xs) where
  type RecordOf (PRecord xs) = xs
  toRecord = unExpr
  fromRecord = unExpr
  
instance IsPRecord (PNewtype (PRecord xs)) where
  type RecordOf (PNewtype (PRecord xs)) = xs
  toRecord = unExpr . pfromNewtype
  fromRecord = unExpr . pnewtype

-- type XS = '[("slava" := PBool),("rossii" := PInteger)]

-- foo :: forall c s. Term 'Expr s (PHList XS) -> Term c s PBool 
-- foo xs = getElem @"slava" xs

-- instance 
--   ( n ~ IndexOf label xs 0
--   , KnownNat n
--   , Nth n xs ~ (LabeledTerm label a))
--   => IsLabel symbol a where
type RecExample = '["suck" ':= PInteger, "dick" ':= PBool]

q :: Term 'Expr s (PRecord '["suck" ':= PInteger, "dick" ':= PBool])
  -> Term 'Expr s PInteger
q rc = get @"suck" rc -- getRecordField @"suck" rc

-- instance IsLabel field (Proxy field,GetRecordField field) where
--   fromLabel = (Proxy @field, MkGetRecordField $ getRecordField @field)

-- field :: forall field. (Proxy field, GetRecordField field) -> GetRecordFieldType field
-- field (Proxy, f) = unGetRecordField f

-- newtype GetRecordField field = MkGetRecordField
--   { unGetRecordField :: GetRecordFieldType field
--   }

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


type GetRecordFieldType label =
  forall fields a s c xs n.
  ( n ~ IndexOf label xs 0
  , KnownNat n
  , UnRecordFields fields ~ xs
  , Nth n xs ~ (LabeledTerm label a)) =>
  Term 'Expr s (PRecord fields) ->
  Term c s a

field :: forall label. GetRecordFieldType label
field = getLabeledElem @label . unRecord

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

-- type family IsJust (a :: Maybe k) where
--   IsJust ('Just _) = 'True
--   IsJust 'Nothing = 'False

type IndexOf :: Symbol -> [a] -> Nat -> Nat
type family IndexOf label xs n = n' where
  IndexOf label (LabeledTerm label a : xs) n = n
  IndexOf label (LabeledTerm _ _ : xs) n = IndexOf label xs (n + 1)
  IndexOf _ _ _ = TypeError ('Text "There is no such label")





