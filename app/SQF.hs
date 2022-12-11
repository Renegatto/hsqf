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
bind ident expr = ident <> " = " <> parens expr <> ";"

indent n = replicate n ' '

compileBlock lvl statements =
  let compile stmt = indent lvl <> compileStatement (succ lvl) stmt
      compiled = intercalate [nl] $ compile <$> statements
  in "{" <> [nl] <> compiled <> "}"

compileStatement :: Int -> Statement -> String
compileStatement lvl = \case
  Seq st0 st1 -> mconcat
    [ compileStatement lvl st0, [nl]
    , compileStatement lvl st1
    ]
  BindLocally name definition ->
    bind ("private _" <> name) (compileExpr lvl definition)
  Bind name definition -> bind name (compileExpr lvl definition) 
  -- optional, makes code less ugly, allows nesting
  ExprStat procedure -> compileExpr lvl procedure

  -- where
  --   block (Procedure stat) =
  --     "{" <> [nl] <> compileStatement (succ lvl) stat <> "}"
  --   block definition = compileExpr definition

compileExpr :: Int -> Expression -> String
compileExpr lvl = \case
  ListLit exprs ->
    "[" <> intercalate "," (compileExpr lvl <$> exprs) <> "]"
  NumLit n -> show n
  StringLit str -> ['"'] <> str <> ['"']

  UnaryOperator opVarid arg -> opVarid <> " " <> parens (compileExpr lvl arg)
  Call fnVarid args -> fnVarid <> " call " <> parens (compileExpr lvl args)
  LocalVar varid -> "_" <> varid
  GlobalVar varid -> varid
  Procedure statements -> compileBlock (succ lvl) statements

-- _this addEventHandler ["fired", { (_this select 0) setvehicleammo 1} ]; 

{-
>>> compileExpr a
addEventHandler call (
  [_this
  ,"fired"
  ,{setvehicleammo call ([select call ([_this,0.0]),1.0])}
  ])


-}

call :: String -> [Expression] -> Expression
call varid = Call varid . ListLit

a :: Expression
a =
  call
    "addEventHandler"
    [ LocalVar "this"
    , StringLit "fired"
    , Procedure
      [ ExprStat
          ( call "setvehicleammo"
              [ call "select" [ LocalVar "this", NumLit 0]
              , NumLit 1
              ]
          )
      ]
    ]
{-
private _reg = { 
  private _arty = vehicle _this; 
  private _reload = { params ["_unit"]; _unit setvehicleammo 1}; 
  _arty addEventHandler ["fired",_reload]; 
}; 
{_x call _reg} forEach (units this);
-}

{-
>>> compileStatement 0 q
private _reg = ({
 private _arty = (vehicle (_this));
 private _reload = ({
   params ([_unit])});
 setvehicleammo call ([1.0])});
forEach call ([{
 _x call ([_reg])}])"
-}
a #> b = Seq a b
expr = ExprStat

q :: Statement
q =
  BindLocally "reg"
    ( Procedure
        [ BindLocally "arty" 
            . UnaryOperator "vehicle"
            $ LocalVar "this"
        , BindLocally "reload" $
            Procedure
              [ expr $ UnaryOperator "params" $ ListLit [LocalVar "unit"]
              ]
        , expr $ call "setvehicleammo" [NumLit 1]
        ]
  )
  #> ExprStat
      ( call "forEach"
          [ Procedure
              [ expr $ call "_x" [LocalVar "reg"]
              ]
          ]
      )

data Expression
  = ListLit [Expression]
  | NumLit Float
  | StringLit String
  | UnaryOperator String Expression
  | Call String Expression
  | LocalVar String
  | GlobalVar String
  | Procedure [Statement]

data Statement
  = Seq Statement Statement
  | ExprStat Expression
  | BindLocally String Expression
  | Bind String Expression
