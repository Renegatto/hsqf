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
module USQF (SQF(..)) where
import Data.Kind (Type)
import Data.List (intercalate)
import SQF qualified (Statement)

-- | Untyped (lmao) SQF language
data SQF
  = ListLit [SQF]
  | NumLit Float
  | StringLit String
  | UnaryOperator String SQF
  | BinaryOperator String SQF SQF
  | Call SQF SQF
  | LocalVar String
  | GlobalVar String
  | Procedure [SQF]
  | Seq SQF SQF
  | BindLocally String SQF
  | Bind String SQF
  | If SQF SQF SQF
  deriving Show