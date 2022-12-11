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
module SQF where
import Data.Kind (Type)
import Data.List (intercalate)
nl = '\n'
eol = [';',nl]
parens x = "(" <> x <> ")"
bind ident expr = ident <> " = " <> parens expr <> eol

indent n = replicate n ' '

compileStatement :: Int -> Statement -> String
compileStatement lvl = \case
  SeqStat st0 st1 -> mconcat
    [ indent lvl, compileStatement lvl st0, eol
    , indent lvl, compileStatement lvl st1, eol
    ]
  LocalStat name definition ->
    bind ("_" <> name) (compileExpr definition)
  BindingStat name definition ->
    bind name (compileExpr definition) 
  -- optional, makes code less ugly, allows nesting
  ExprStat (ProcedureExpr stat) ->
    nl : compileStatement (succ lvl) stat
  ExprStat expr -> compileExpr expr

compileExpr :: Expression -> String
compileExpr = \case
  ListLitExpr exprs ->
    "[" <> intercalate "," (compileExpr <$> exprs) <> "]"
  NumLitExpr n -> show n
  StringLitExpr str -> ['"'] <> str <> ['"']

  UnaryOpExpr opVarid arg -> opVarid <> " " <> parens (compileExpr arg)
  CallExpr fnVarid args -> fnVarid <> " call " <> parens (compileExpr args)
  LocalIdentExpr varid -> "_" <> varid
  GlobalIdentExpr varid -> varid
  ProcedureExpr stat -> compileStatement 0 stat

data Expression
  = ListLitExpr [Expression]
  | NumLitExpr Float
  | StringLitExpr String
  | UnaryOpExpr String Expression
  | CallExpr String Expression
  | LocalIdentExpr String
  | GlobalIdentExpr String
  | ProcedureExpr Statement

data Statement
  = SeqStat Statement Statement
  | ExprStat Expression
  | LocalStat String Expression
  | BindingStat String Expression
