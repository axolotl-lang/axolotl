module Transpiler.Backends.JS.JS where

import Analyser.Util as AU
import Data.Foldable (foldlM)
import Data.HashTable.IO as H
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Numeric as T
import Parser.Ast
import TextShow (TextShow (showt))

-- TODO remove gd, ld, and move out of IO

pshowt :: (TextShow a) => a -> IO T.Text
pshowt = pure . showt

type IndentLevel = Int

type Backend = Expr -> AU.GDefs -> AU.LDefs -> IndentLevel -> IO T.Text

jsBackend :: Backend
jsBackend e@(Root exprs) gd ld il = do
  transpiled <- mapM (\x -> jsBackend x gd ld il) exprs
  pure $ foldl (\acc curr -> acc <> "\n" <> AU.repeatText "\t" il <> curr) "" transpiled

-- Literals
jsBackend e@(IntLiteral i) gd ld il = pshowt i
jsBackend e@(FloatLiteral f) gd ld il = pshowt f
jsBackend e@(CharLiteral c) gd ld il = pshowt c
jsBackend e@(StrLiteral s) gd ld il = pshowt s
jsBackend e@(BoolLiteral b) gd ld il = pshowt b
jsBackend e@Nil gd ld il = do
  pure "null"

--
jsBackend e@(Array arr) gd ld il = do
  v <-
    foldl
      ( \acc curr -> do
          tr <- jsBackend curr gd ld 0
          acc <- acc
          pure $ acc <> tr <> ", "
      )
      (pure "[" :: IO T.Text)
      arr
  pure $ v <> "]"

--
jsBackend e@(VariableUsage v) gd ld il = do
  pshowt v

--
jsBackend e@(VariableDef name _ val) gd ld il = do
  val <- jsBackend val gd ld il
  pure $ "const " <> name <> " = " <> val <> ";"

--
jsBackend e@(Unary _ expr) gd ld il = do
  expr <- jsBackend expr gd ld il
  pure $ "-" <> expr

--
jsBackend e@(FunctionDef name ret args body native) gd ld il = undefined