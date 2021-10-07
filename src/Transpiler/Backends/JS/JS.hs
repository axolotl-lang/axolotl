module Transpiler.Backends.JS.JS where

import Analyser.Util as AU
import Data.Foldable (foldlM)
import Data.HashTable.IO as H
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Numeric as T
import Parser.Ast
import TextShow (TextShow (showt))

type IndentLevel = Int

type Backend = Expr -> IndentLevel -> T.Text

jsBackend :: Backend
jsBackend e@(Root exprs) il = do
  let transpiled = map (`jsBackend` il) exprs
  foldl (\acc curr -> acc <> "\n" <> AU.repeatText "\t" il <> curr) "" transpiled

-- Literals
jsBackend e@(IntLiteral i) il = showt i
jsBackend e@(FloatLiteral f) il = showt f
jsBackend e@(CharLiteral c) il = showt c
jsBackend e@(StrLiteral s) il = showt s
jsBackend e@(BoolLiteral b) il = showt b
jsBackend e@Nil il = do
  "null"

--
jsBackend e@(Array arr) il = do
  foldl
    ( \acc curr ->
        acc <> jsBackend curr 0 <> ", "
    )
    "["
    arr
    <> "]"

--
jsBackend e@(VariableUsage v) il = do
  showt v

--
jsBackend e@(VariableDef name _ val) il = do
  "const " <> name <> " = " <> jsBackend val il <> ";"

--
jsBackend e@(Unary _ expr) il = do
  "-" <> jsBackend expr il

--
jsBackend e@(FunctionDef name ret args body native) il = undefined