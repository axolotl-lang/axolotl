module Parser.Parser where

import Data.Either.Combinators (isRight)
import Parser.Ast (Expr (..), UnaryOp (Neg))
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
import Text.Megaparsec (MonadParsec (observing, try), many, single, (<|>))
import Text.Megaparsec.Char (numberChar)

intLiteral :: Parser Expr
intLiteral = intLit >>= \x -> pure $ IntLiteral x

floatLiteral :: Parser Expr
floatLiteral = floatLit >>= \x -> pure $ FloatLiteral x

charLiteral :: Parser Expr
charLiteral = charLit >>= \x -> pure $ CharLiteral x

strLiteral :: Parser Expr
strLiteral = strLit >>= \x -> pure $ StrLiteral x

boolLiteral :: Parser Expr
boolLiteral = boolLit >>= \x -> pure $ BoolLiteral x

arrayLiteral :: Parser Expr
arrayLiteral = squares arrExp
  where
    arrExp = commaSep arraySupportedExpr >>= \x -> pure $ Array x

nil :: Parser Expr
nil = nilLit >> pure Nil

variable :: Parser Expr
variable = identifier >>= \x -> pure $ Variable x

unary :: Parser Expr
unary = single '-' >> try floatLiteral <|> intLiteral >>= \num -> pure $ Unary Neg num

variableDef :: Parser Expr
variableDef =
  parens $
    rword "def" >> optionallyTypedIdentifier >>= \x ->
      expr >>= \y ->
        pure $ uncurry VariableDef x y

foreignFunctionDef :: Parser Expr
foreignFunctionDef = parens func
  where
    func = do
      rword "foreign defun"
      id <- identifierWithType
      pure $ uncurry FunctionDef id [] [] True

functionDef :: Parser Expr
functionDef = parens func
  where
    func = do
      rword "defun"
      id <- optionallyTypedIdentifier
      args <- squares $ many identifierWithType
      body <- braces exprs
      pure $ uncurry FunctionDef id args body False

arbitraryBlock :: Parser Expr
arbitraryBlock = braces $ many expr >>= \x -> pure $ ArbitraryBlock x

functionCall :: Parser Expr
functionCall = parens identifier >>= \x -> FunctionCall x <$> exprs

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
  try floatLiteral
    <|> intLiteral
    <|> charLiteral
    <|> strLiteral
    <|> boolLiteral
    <|> arrayLiteral
    <|> nil
    <|> variable
    <|> try functionDef
    <|> try foreignFunctionDef
    <|> try variableDef
    <|> try conditional
    <|> functionCall
    <|> arbitraryBlock
    <|> unary

exprs :: Parser [Expr]
exprs = many expr

root :: Parser Expr
root = exprs >>= \x -> pure $ Root x
