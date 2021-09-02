module Analyser.Util where

import Data.Either.Combinators (maybeToLeft, maybeToRight)
import Data.HashMap.Strict as H (HashMap, lookup)
import Data.Text as T (Text, pack)
import Parser.Ast
  ( Expr (..),
    VDataType (ArrayOf, Bool, Float, Function, Inferred, Int, NilType, String),
  )

data Def
  = Variable VDataType Expr
  | Function VDataType [(Text, VDataType)] [Expr] Bool
  | Argument VDataType
  deriving (Show, Eq)

type GDefs = HashMap Text Def

type LDefs = HashMap Text GDefs

rFoldl :: Foldable t => t a -> b -> (b -> a -> b) -> b
rFoldl list def fun = foldl fun def list

tfst :: (a, b, c) -> a
tfst (x, _, _) = x

tsnd :: (a, b, c) -> b
tsnd (_, y, _) = y

tthd :: (a, b, c) -> c
tthd (_, _, z) = z

getTypeFromArr :: VDataType -> VDataType
getTypeFromArr (ArrayOf x) = x
getTypeFromArr y = error "getTypeFromArr is only for ArrayOf"

getTypeFromExpr :: Expr -> GDefs -> Either Text VDataType
getTypeFromExpr ex gd = case ex of
  IntLiteral {} -> Right Int
  FloatLiteral {} -> Right Float
  CharLiteral {} -> Right Int
  StrLiteral {} -> Right String
  BoolLiteral {} -> Right Bool
  Array exs -> getTypeFromExpr (head exs) gd >>= \t -> Right $ ArrayOf t
  Nil -> Right NilType
  Parser.Ast.Variable name -> do
    def <- maybeToRight ("use of undefined variable '" <> name <> "'") (H.lookup name gd)
    case def of
      Analyser.Util.Variable _ expr -> getTypeFromExpr expr gd
      Analyser.Util.Function _ args expr frgn -> undefined -- TODO
      Analyser.Util.Argument vdt -> Right vdt
  FunctionCall name args -> do
    def <- maybeToRight ("call to undefined function '" <> name <> "'") (H.lookup name gd)
    case def of
      Analyser.Util.Variable v _ -> case v of
        Parser.Ast.Function args ret -> Right ret
        x -> Left $ "Variable of type '" <> pack (show x) <> "' is not callable"
      Analyser.Util.Function vdt _ _ _ -> Right vdt
      Analyser.Util.Argument vdt -> undefined -- TODO
  VariableDef {} -> Right NilType
  FunctionDef {} -> Right NilType
  AnonymousFunction {} -> undefined -- TODO
  Root {} -> Left "Unexpected root"
  Unary _ ex -> case ex of
    IntLiteral n -> Right Int
    FloatLiteral x -> Right Float
    _ -> Left "Unary operator applied to unexpected expression"