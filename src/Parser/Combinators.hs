module Parser.Combinators where

import Control.Monad (void)
import Data.Char (ord)
import Data.Either (isRight)
import Data.Functor ((<&>))
import Data.String.Conversions (cs)
import Data.Text as T (Text, pack, unpack)
import qualified Data.Text as T
import Data.Void (Void)
import Parser.Ast
  ( VDataType (Any, ArrayOf, Bool, Float, Inferred, Int, NilType, String),
  )
import Text.Megaparsec
  ( MonadParsec (notFollowedBy, observing, takeWhileP, try),
    Parsec,
    between,
    many,
    satisfy,
    sepBy,
    single,
    (<|>),
  )
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string, symbolChar)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- Foundations
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment ";"
    blockCmnt = L.skipBlockComment "#|" "|#"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

symbol' :: Char -> Parser ()
symbol' s = void $ sc >> single s

symbol'' :: Char -> Parser ()
symbol'' s = single s >> sc

-- Utility
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

squares :: Parser a -> Parser a
squares = between (symbol "[") (symbol "]")

dquotes :: Parser a -> Parser a
dquotes = between (symbol' '"') (symbol'' '"')

squotes :: Parser a -> Parser a
squotes = between (symbol' '\'') (symbol'' '\'')

comma :: Parser ()
comma = void $ symbol ","

colon :: Parser ()
colon = void $ symbol ":"

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p (symbol ",")

-- Literals
intLit :: Parser Int
intLit = lexeme L.decimal

floatLit :: Parser Double
floatLit = lexeme L.float

nilLit :: Parser ()
nilLit = void $ symbol "nil"

strLit :: Parser Text
strLit =
  dquotes (takeWhileP Nothing (/= '"')) >>= \c ->
    pure $ T.pack (read ('"' : cs c <> "\""))

charLit :: Parser Int
charLit =
  squotes $
    (ord <$> satisfy (`notElem` ['\\', '\'']))
      <|> (single '\\' >> intLit)

boolLit :: Parser Bool
boolLit =
  observing (symbol "true") >>= \case
    Left _ -> symbol "false" >> pure False
    Right _ -> pure True

-- Utils
rword :: String -> Parser ()
rword w = (lexeme . try) (string (pack w) *> notFollowedBy alphaNumChar)

mathSymbol :: Parser Char
mathSymbol = single '+' <|> single '-' <|> single '/' <|> single '%' <|> single '*' <|> single '>' <|> single '<' <|> single '=' <|> single '!'

identifier :: Parser Text
identifier = (lexeme . try) (p <&> pack)
  where
    p = (:) <$> (letterChar <|> mathSymbol) <*> many (alphaNumChar <|> single '-' <|> single '?' <|> single '_' <|> single '=')

getTypeFromStr :: Text -> Parser VDataType
getTypeFromStr "string" = pure String
getTypeFromStr "int" = pure Int
getTypeFromStr "float" = pure Float
getTypeFromStr "bool" = pure Bool
getTypeFromStr "nil" = pure NilType
getTypeFromStr "inferred" = pure Inferred
getTypeFromStr "any" = pure Any
getTypeFromStr "function" = fail "not yet supported"
getTypeFromStr x =
  if T.take 2 (T.reverse x) == "]["
    then getTypeFromStr (T.take (T.length x - 2) x) >>= \x -> pure (ArrayOf x)
    else fail "type does not exist"

identifierWithType :: Parser ((Text, VDataType), Bool)
identifierWithType = do
  isVariadic <- isRight <$> observing (symbol "&")
  args <- parens iwt
  pure (args, isVariadic)
  where
    iwt = do
      id <- identifier
      symbol ":"
      vtype <- getTypeFromStr <$> takeWhileP Nothing (\x -> (x /= ')') && (x /= ','))
      vtype >>= \x -> pure (id, x)

optionallyTypedIdentifier :: Parser (Text, VDataType)
optionallyTypedIdentifier =
  try (fst <$> identifierWithType)
    <|> (identifier >>= \x -> pure (x, Inferred))