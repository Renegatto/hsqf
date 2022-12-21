{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}

module HSQF.Definition
  ( S,
    PType,
    Term (MkTerm, runTerm),
    Scope (Stat, Expr),
    (:==>),
    (:-->),
  )
where

import Data.Data (Proxy (Proxy))
import Data.Kind (Type)
import Data.List (intercalate)
import GHC.TypeLits (natVal, type (-))
import GHC.TypeNats (KnownNat)
import SQF
  ( SQF
      ( BinaryOperator,
        Call,
        GlobalVar,
        If,
        ListLit,
        NumLit,
        StringLit,
        UnaryOperator
      ),
  )
import qualified SQF
import Unsafe.Coerce (unsafeCoerce)

data S
type PType = S -> Type

type Scope :: Type
data Scope = Expr | Stat

type ClosedTerm = forall s. Term s

type Term :: Scope -> S -> PType -> Type
newtype Term c s a = MkTerm { runTerm :: Int -> SQF }

-- | SQF Unary Procedure (CODE)
type (:-->) :: PType -> PType -> PType
newtype (:-->) a b s = MkUProcedure
  { runUProcedure :: forall c. Term c s (a :--> b)}

-- | SQF Nary Procedure (CODE)
type (:==>) :: [PType] -> PType -> PType
newtype (:==>) args b s = MkProcedure
  { runProcedure :: forall c. Term c s (args :==> b)}