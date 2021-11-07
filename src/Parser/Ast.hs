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
  | Function [VDataType] VDataType Bool
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
  | VariableUsage Text -- like username
  | VariableDef {name :: Text, vtype :: VDataType, val :: Expr} -- like (def username 20)
  | Unary UnaryOp Expr -- only - (for now)
  -- like (defun (function-name: VDataType) (arg1: DataType, arg2: DataType) { ... })
  -- the bool is true if the function is a native function
  | FunctionDef
      { name :: Text,
        returnType :: VDataType,
        -- the bool signifies whether the last argument
        -- of the function is a variadic argument
        expectedArgs :: ([(Text, VDataType)], Bool),
        body :: [Expr],
        isNative :: Bool
      }
  | FunctionCall {name :: Text, actualArgs :: [Expr]} -- like (print "hello")
  | AnonymousFunction {returnType :: VDataType, expectedArgs :: ([(Text, VDataType)], Bool), body :: [Expr]}
  | ArbitraryBlock [Expr] -- a random block of exprs like { 2 }
  | Conditional {cond :: Expr, ifTrue :: Expr, ifFalse :: Expr} -- (if cond iftrue iffalse)
  | Root [Expr] -- a set of all exprs in file
  deriving (Show, Eq)