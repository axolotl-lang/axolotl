module Parser.Ast where

import Data.Text (Text)

data UnaryOp = Neg
  deriving (Show, Eq)

data VDataType
  = String
  | Int
  | Float
  | Bool
  | NilType
  | Function [VDataType] VDataType
  | ArrayOf VDataType
  | Inferred
  deriving (Show, Eq)

data Expr
  = IntLiteral Int -- like 3
  | FloatLiteral Double -- like 3.52 (floats are internally doubles for accuracy)
  | CharLiteral Int -- like 'a'
  | StrLiteral Text -- like "hello"
  | BoolLiteral Bool -- like true
  | Array [Expr]
  | Nil -- only nil
  | Variable Text -- like username
  | VariableDef Text VDataType Expr -- like (def username 20)
  | Unary UnaryOp Expr -- only - (for now)
  -- like (defun (function-name: VDataType) (arg1: DataType, arg2: DataType) { ... })
  -- the bool is true if the function is a native function
  | FunctionDef Text VDataType [(Text, VDataType)] [Expr] Bool
  | FunctionCall Text [Expr] -- like (print "hello")
  | AnonymousFunction VDataType [(Text, VDataType)] [Expr]
  | ArbitraryBlock [Expr] -- a random block of exprs like { 2 }
  | Conditional Expr Expr Expr -- (if cond iftrue iffalse)
  | Root [Expr] -- a set of all exprs in file
  deriving (Show, Eq)