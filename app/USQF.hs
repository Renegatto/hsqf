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
module USQF (SQF(..),compile,unNewLine) where
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

nl = '\n'
eol = [';',nl]
parens x = "(" <> x <> ")"
bind ident expr = ident <> " = " <> parens expr <> ";"

indent n = replicate n ' '

unNewLine :: String -> String
unNewLine = fmap $ \case
  '\n' -> ' '
  x -> x

compileBlock :: Int -> [SQF] -> String
compileBlock lvl statements =
  let compile' stmt = indent lvl <> compile (succ lvl) stmt <> eol
      compiled = foldMap compile' statements
  in "{" <> [nl] <> compiled <> "}"

compile :: Int -> SQF -> String
compile lvl = \case
  Seq st0 st1 -> mconcat
    [ compile lvl st0, [nl]
    , indent lvl <> compile lvl st1
    ]
  BindLocally name definition ->
    bind ("private _" <> name) (compile lvl definition)
  Bind name definition -> bind name (compile lvl definition) 
  ListLit exprs ->
    "[" <> intercalate "," (compile lvl <$> exprs) <> "]"
  NumLit n -> show n
  StringLit str -> ['"'] <> str <> ['"']
  UnaryOperator opVarid arg -> opVarid <> " " <> parens (compile lvl arg)
  BinaryOperator op arg0 arg1 ->
    unwords
      [ parens $ compile lvl arg0
      , op
      , parens $ compile lvl arg1
      ]
  Call fn args -> compile lvl $ BinaryOperator "call" fn args
  If boolExpr ifTrue ifFalse ->
    unwords
      [ "if"
      , parens $ compile lvl boolExpr
      , "then{"
      , compile lvl ifTrue
      , "}else{"
      , compile lvl ifFalse
      , "};"
      ]
  LocalVar varid -> "_" <> varid
  GlobalVar varid -> varid
  Procedure statements -> compileBlock (succ lvl) statements