{-# LANGUAGE TemplateHaskell #-}

module Transpiler.Backends.JS.JS where

import Data.Foldable (foldlM)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as B
import Data.FileEmbed (embedFile)
import Parser.Ast
  ( Expr
      ( AnonymousFunction,
        ArbitraryBlock,
        Array,
        BoolLiteral,
        CharLiteral,
        Conditional,
        FloatLiteral,
        FunctionCall,
        FunctionDef,
        IntLiteral,
        Nil,
        Root,
        StrLiteral,
        Unary,
        VariableDef,
        VariableUsage
      ),
  )
import TextShow (TextShow (showt))
import Transpiler.Util (makeCommaSep, repeatText)

type IndentLevel = Int

type Backend = Expr -> IndentLevel -> T.Text

jsStdlib :: T.Text
jsStdlib = B.decodeUtf8 $(embedFile "src/Transpiler/Backends/JS/stdlib.js")

tab :: T.Text
tab = repeatText " " 4

sanitiseDefinition :: T.Text -> T.Text
sanitiseDefinition = T.replace "-" "_"

sanitiseFunctionCall :: T.Text -> T.Text
sanitiseFunctionCall "+i" = "__plus__int"
sanitiseFunctionCall "-i" = "__minus__int"
sanitiseFunctionCall "+f" = "__plus__float"
sanitiseFunctionCall "-f" = "__minus__float"
sanitiseFunctionCall "*i" = "__multiply__int"
sanitiseFunctionCall "*f" = "__multiply__float"
sanitiseFunctionCall "/i" = "__divide__int"
sanitiseFunctionCall "/f" = "__divide__float"
sanitiseFunctionCall "str" = "__str"
sanitiseFunctionCall "print" = "__print"
sanitiseFunctionCall y = sanitiseDefinition y

needSemicolon :: Expr -> Bool
needSemicolon (FunctionCall {}) = True
needSemicolon (VariableDef {}) = True
needSemicolon _ = False

returningRoot :: Backend
returningRoot e@(Root exprs) il =
  do
    let ret = jsBackend (last exprs) il
    let t = reverse (drop 1 (reverse exprs))
    let res =
          foldl
            ( \acc curr -> do
                let res = acc <> "\n" <> repeatText tab il <> jsBackend curr il
                if needSemicolon curr then res <> ";" else res
            )
            ""
            t
    res <> "\n" <> repeatText tab il <> "return " <> ret <> ";"

jsBackend :: Backend
jsBackend e@(Root exprs) il = do
  foldl
    ( \acc curr -> do
        let res = acc <> "\n" <> repeatText tab il <> jsBackend curr il
        if needSemicolon curr then res <> ";" else res
    )
    ""
    exprs

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
        acc <> jsBackend curr 0 <> ","
    )
    "["
    arr
    <> "]"

--
jsBackend e@(VariableUsage v) il = do
  sanitiseDefinition v

--
jsBackend e@(VariableDef name _ val) il = do
  "const " <> sanitiseDefinition name <> " = " <> jsBackend val il

--
jsBackend e@(Unary _ expr) il = do
  "-" <> jsBackend expr il

--
jsBackend e@(FunctionDef name _ args body native) il = do
  let header = "\nfunction " <> sanitiseDefinition name
  let args' = makeCommaSep "(" (map (sanitiseDefinition . fst) (fst args)) ")"
  let body' = " {" <> returningRoot (Root body) (il + 1) <> "\n}"
  header <> args' <> body'

--
jsBackend e@(FunctionCall name actualArgs) il = do
  let args' = makeCommaSep "(" (map (`jsBackend` il) actualArgs) ")"
  sanitiseFunctionCall name <> args'

--
jsBackend e@(AnonymousFunction _ args body) il = do
  let args' = makeCommaSep "(" (map fst (fst args)) ") => "
  let body' = " {" <> returningRoot (Root body) (il + 1) <> "\n}"
  args' <> body'

--
jsBackend e@(ArbitraryBlock body) il = do
  "(() => {" <> returningRoot (Root body) (il + 1) <> "\n})()"

--
jsBackend e@(Conditional cond ift iff) il = do
  let cond' = jsBackend cond il
  let ift' = jsBackend ift il
  let iff' = jsBackend iff il
  "if (" <> cond' <> ") {" <> ift' <> "} else {" <> iff' <> "}"