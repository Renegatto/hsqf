{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module SQF (SQF (..), compile, unNewLine) where

import Data.Function (fix)
import Data.Kind (Type)
import Data.List (intercalate)

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
  deriving (Show)

nl = '\n'

eol = [';', nl]

parens x = "(" <> x <> ")"

bind ident expr = ident <> " = " <> expr <> ";"

indent n = replicate n ' '

unNewLine :: String -> String
unNewLine = fmap $ \case
  '\n' -> ' '
  x -> x

compileBlock :: (Int -> SQF -> String) -> Int -> [SQF] -> String
compileBlock compiler lvl statements =
  let compile' stmt = indent lvl <> compiler (succ lvl) stmt <> eol
      compiled = foldMap compile' statements
   in "{" <> [nl] <> compiled <> "}"

compile :: Int -> SQF -> String
compile = fix compileWithLessParens

compileWithLessParens :: (Int -> SQF -> String) -> Int -> SQF -> String
compileWithLessParens self lvl = \case
  Seq st0 st1 ->
    mconcat
      [ self lvl st0,
        [nl],
        indent lvl <> self lvl st1
      ]
  BindLocally name definition ->
    bind ("private _" <> name) (self lvl definition)
  Bind name definition -> bind name (self lvl definition)
  ListLit exprs ->
    "[" <> intercalate "," (self lvl <$> exprs) <> "]"
  NumLit n -> show n
  StringLit str -> ['"'] <> str <> ['"']
  UnaryOperator opVarid arg ->
    parens $
      opVarid <> " " <> self lvl arg
  BinaryOperator op arg0 arg1 ->
    parens $
      unwords
        [ self lvl arg0,
          op,
          self lvl arg1
        ]
  Call fn args -> self lvl $ BinaryOperator "call" fn args
  If boolExpr ifTrue ifFalse ->
    unwords
      [ "if",
        parens $ self lvl boolExpr,
        "then{",
        self lvl ifTrue,
        "}else{",
        self lvl ifFalse,
        "};"
      ]
  LocalVar varid -> "_" <> varid
  GlobalVar varid -> varid
  Procedure statements -> compileBlock self (succ lvl) statements