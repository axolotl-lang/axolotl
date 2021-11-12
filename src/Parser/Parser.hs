module Parser.Parser where

import Analyser.Util (rFoldl)
import Data.Either.Combinators (isRight)
import Data.Functor ((<&>))
import qualified Data.Text as T
import Parser.Ast (Expr (..), UnaryOp (Neg), VDataType)
import Parser.Combinators
  ( Parser,
    boolLit,
    braces,
    charLit,
    commaSep,
    floatLit,
    identifier,
    identifierWithType,
    intLit,
    nilLit,
    optionallyTypedIdentifier,
    parens,
    rword,
    squares,
    strLit,
  )
import Sugar.Sugar (desugarExpr)
import Text.Megaparsec (MonadParsec (observing, try), many, single, (<|>))
import Text.Megaparsec.Char (numberChar)

intLiteral :: Parser Expr
intLiteral = intLit <&> IntLiteral

floatLiteral :: Parser Expr
floatLiteral = floatLit <&> FloatLiteral

charLiteral :: Parser Expr
charLiteral = charLit <&> CharLiteral

strLiteral :: Parser Expr
strLiteral = strLit <&> StrLiteral

boolLiteral :: Parser Expr
boolLiteral = boolLit <&> BoolLiteral

arrayLiteral :: Parser Expr
arrayLiteral = squares arrExp
  where
    arrExp = commaSep arraySupportedExpr <&> Array

nil :: Parser Expr
nil = nilLit >> pure Nil

variable :: Parser Expr
variable = identifier <&> VariableUsage

unary :: Parser Expr
unary = single '-' >> try floatLiteral <|> intLiteral <&> Unary Neg

variableDef :: Parser Expr
variableDef =
  parens $
    rword "def" >> optionallyTypedIdentifier >>= \x ->
      expr <&> uncurry VariableDef x

nativeFunctionDef :: Parser Expr
nativeFunctionDef = parens func
  where
    func = do
      rword "native defun"
      id <- identifierWithType
      args <- squares $ many identifierWithType
      let len = length args
      -- check if only the last element is variadic
      fnTransformer True (fst id) args

-- (defun some-fn [(arg1: int, arg2: int, ...)] { ... })
-- can also have variadic arguments, for example arg3 in
-- the below example will be an array of all the strings passed
-- (defun some-fn [(arg1: int) (arg2: int) &(arg3: string)] {
--    (print arg1)
--    (print arg2)
--    (print arg3)
-- })
-- (some-fn 1 2 "hi" "hello" "bye")
-- prints: 1 2 ["hi", "hello", "bye"]
-- a variadic argument must be the last in a function
functionDef :: Parser Expr
functionDef = parens func
  where
    func = do
      rword "defun"
      id <- optionallyTypedIdentifier
      args <- squares $ many identifierWithType
      -- check if only the last element is variadic
      fnTransformer False id args

fnTransformer :: Bool -> (T.Text, VDataType) -> [((T.Text, VDataType), Bool)] -> Parser Expr
fnTransformer isNative id args = do
  let len = length args
  args' <- rFoldl
    (zip [(1 :: Int) ..] args)
    (pure ([], False) :: Parser ([(T.Text, VDataType)], Bool))
    $ \acc curr -> do
      acc <- acc
      let val = fst acc <> [(fst . snd) curr]
      -- if current argument is variadic
      if (snd . snd) curr
        then -- check if it's the last element

          if fst curr == len
            then pure (val, True)
            else fail "Only the last argument of a function can be variadic!"
        else -- if not variadic, just add to accumulator
          pure (val, False)
  body <- if isNative then pure [] else braces exprs
  pure $ uncurry FunctionDef id args' body isNative

arbitraryBlock :: Parser Expr
arbitraryBlock = braces $ many expr <&> ArbitraryBlock

functionCall :: Parser Expr
functionCall = parens $ (identifier <&> FunctionCall) <*> exprs

conditional :: Parser Expr
conditional = parens $ rword "if" >> Conditional <$> expr <*> expr <*> expr

arraySupportedExpr :: Parser Expr
arraySupportedExpr =
  try floatLiteral
    <|> intLiteral
    <|> charLiteral
    <|> strLiteral
    <|> boolLiteral
    <|> arrayLiteral
    <|> nil
    <|> variable
    <|> functionCall
    <|> unary

expr :: Parser Expr
expr =
  desugarExpr <$> exprOptions
  where
    exprOptions =
      try floatLiteral
        <|> intLiteral
        <|> charLiteral
        <|> strLiteral
        <|> boolLiteral
        <|> arrayLiteral
        <|> nil
        <|> variable
        <|> try functionDef
        <|> try nativeFunctionDef
        <|> try variableDef
        <|> try conditional
        <|> functionCall
        <|> arbitraryBlock
        <|> unary

exprs :: Parser [Expr]
exprs = many expr

root :: Parser Expr
root = exprs <&> Root
