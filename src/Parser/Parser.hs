module Parser.Parser where

import Analyser.Util (rFoldl)
import Data.Either.Combinators (isRight)
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
variable = identifier >>= \x -> pure $ VariableUsage x

unary :: Parser Expr
unary = single '-' >> try floatLiteral <|> intLiteral >>= \num -> pure $ Unary Neg num

variableDef :: Parser Expr
variableDef =
  parens $
    rword "def" >> optionallyTypedIdentifier >>= \x ->
      expr >>= \y ->
        pure $ uncurry VariableDef x y

nativeFunctionDef :: Parser Expr
nativeFunctionDef = parens func
  where
    func = do
      rword "native defun"
      id <- identifierWithType
      args <- squares $ many identifierWithType
      let len = length args
      -- check if only the last element is variadic
      fnTransformer (fst id) args

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
      fnTransformer id args

fnTransformer :: (T.Text, VDataType) -> [((T.Text, VDataType), Bool)] -> Parser Expr
fnTransformer id args = do
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
  body <- braces exprs
  pure $ uncurry FunctionDef id args' body False

arbitraryBlock :: Parser Expr
arbitraryBlock = braces $ many expr >>= \x -> pure $ ArbitraryBlock x

functionCall :: Parser Expr
functionCall = parens $ identifier >>= \x -> FunctionCall x <$> exprs

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
  desugarExpr <$> try floatLiteral
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
exprs = map desugarExpr <$> many expr

root :: Parser Expr
root = exprs >>= \x -> pure $ Root x
