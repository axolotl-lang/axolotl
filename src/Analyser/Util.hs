module Analyser.Util where

import Data.Either.Combinators (maybeToLeft, maybeToRight)
import Data.HashMap.Strict as H (HashMap, empty, lookup)
import Data.Text as T (Text, pack)
import Parser.Ast
  ( Expr (..),
    VDataType (ArrayOf, Bool, Float, Function, Inferred, Int, NilType, String),
  )

data Def
  = Variable VDataType Expr
  | Function VDataType [(Text, VDataType)] [Expr] Bool
  | Argument VDataType
  | -- this is used to allow recursive calls
    -- and to check if they are proper
    IncompleteFunction [(Text, VDataType)]
  deriving (Show, Eq)

type GDefs = HashMap Text Def

type LDefs = HashMap Text GDefs

type Env = (GDefs, LDefs)

type AnalyserResult = [Either Text Expr]

rFoldl :: Foldable t => t a -> b -> (b -> a -> b) -> b
rFoldl list def fun = foldl fun def list

makeLeft :: a -> [Either a b]
makeLeft r = [Left r]

tfst :: (a, b, c) -> a
tfst (x, _, _) = x

tsnd :: (a, b, c) -> b
tsnd (_, y, _) = y

tthd :: (a, b, c) -> c
tthd (_, _, z) = z

getTypeFromArr :: VDataType -> VDataType
getTypeFromArr (ArrayOf x) = x
getTypeFromArr y = error "getTypeFromArr is only for ArrayOf"

isFnCall :: Text -> Expr -> Bool
isFnCall name' expr = case expr of
  FunctionCall name args -> name == name'
  _ -> False

getTypeOfExpr :: Expr -> GDefs -> Either Text VDataType
getTypeOfExpr ex gd = case ex of
  IntLiteral {} -> Right Int
  FloatLiteral {} -> Right Float
  CharLiteral {} -> Right Int
  StrLiteral {} -> Right String
  BoolLiteral {} -> Right Bool
  Array exs -> getTypeOfExpr (head exs) gd >>= \t -> Right $ ArrayOf t
  Nil -> Right NilType
  VariableUsage name -> do
    def <- maybeToRight ("use of undefined variable '" <> name <> "'") (H.lookup name gd)
    case def of
      Analyser.Util.Variable _ expr -> getTypeOfExpr expr gd
      Analyser.Util.Function _ args expr frgn -> undefined -- TODO
      Analyser.Util.Argument vdt -> Right vdt
      Analyser.Util.IncompleteFunction vdt -> undefined -- TODO
  FunctionCall name args -> do
    def <- maybeToRight ("call to undefined function '" <> name <> "'") (H.lookup name gd)
    case def of
      Analyser.Util.Variable v _ -> case v of
        Parser.Ast.Function args ret -> Right ret
        x -> Left $ "Variable of type '" <> pack (show x) <> "' is not callable"
      Analyser.Util.Function vdt _ _ _ -> Right vdt
      Analyser.Util.Argument vdt -> undefined -- TODO
      Analyser.Util.IncompleteFunction vdt -> Right Inferred
  ArbitraryBlock exprs -> getTypeOfExpr (last exprs) gd
  -- semCheckExprs will bail out if type of ift /= type of iff
  Conditional cond ift iff -> getTypeOfExpr ift gd
  VariableDef {} -> Right NilType
  FunctionDef {} -> Right NilType
  AnonymousFunction {} -> undefined -- TODO
  Root {} -> Left "Unexpected root"
  Unary _ ex -> case ex of
    IntLiteral n -> Right Int
    FloatLiteral x -> Right Float
    _ -> Left "Unary operator applied to unexpected expression"