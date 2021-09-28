module Analyser.Util where

import Data.Either.Combinators (maybeToLeft, maybeToRight)
import qualified Data.HashTable.IO as H
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
    IncompleteFunction [(Text, VDataType)] VDataType Bool
  deriving (Show, Eq)

type GDefs = H.BasicHashTable Text Def

type LDefs = H.BasicHashTable Text GDefs

type Env = (GDefs, LDefs)

type AnalyserResult = [Either Text Expr]

rFoldl :: Foldable t => t a -> b -> (b -> a -> b) -> b
rFoldl list def fun = Prelude.foldl fun def list

makeLeft :: a -> [Either a b]
makeLeft r = [Left r]

getTypeFromArr :: VDataType -> VDataType
getTypeFromArr (ArrayOf x) = x
getTypeFromArr y = error "getTypeFromArr is only for ArrayOf"

hUnion :: H.BasicHashTable Text Def -> H.BasicHashTable Text Def -> IO (H.BasicHashTable Text Def)
hUnion a b = H.mapM_ (uncurry (H.insert b)) a >> pure b

hUnion' :: [(Text, Def)] -> H.BasicHashTable Text Def -> IO (H.BasicHashTable Text Def)
hUnion' a b = mapM_ (uncurry (H.insert b)) a >> pure b

isFnCall :: Text -> Expr -> Bool
isFnCall name' expr = case expr of
  FunctionCall name args -> name == name'
  _ -> False

getTypeOfExpr :: Expr -> GDefs -> IO (Either Text VDataType)
getTypeOfExpr ex gd = case ex of
  IntLiteral {} -> pure $ Right Int
  FloatLiteral {} -> pure $ Right Float
  CharLiteral {} -> pure $ Right Int
  StrLiteral {} -> pure $ Right String
  BoolLiteral {} -> pure $ Right Bool
  Array exs -> do
    t' <- getTypeOfExpr (Prelude.head exs) gd
    case t' of
      Left txt -> pure $ Left txt
      Right vdt -> pure $ Right vdt
  Nil -> pure $ Right NilType
  VariableUsage name -> do
    lu <- H.lookup gd name
    let def = maybeToRight ("use of undefined variable '" <> name <> "'") lu
    case def of
      Left txt -> pure $ Left txt
      Right def' -> case def' of
        Analyser.Util.Variable _ expr -> getTypeOfExpr expr gd
        Analyser.Util.Function _ args expr frgn -> undefined -- TODO
        Analyser.Util.Argument vdt -> pure $ Right vdt
        Analyser.Util.IncompleteFunction args vdt native -> undefined -- TODO
  FunctionCall name args -> do
    lu <- H.lookup gd name
    let def = maybeToRight ("call to undefined function '" <> name <> "'") lu
    case def of
      Left txt -> pure $ Left txt
      Right def' -> case def' of
        Analyser.Util.Variable v _ -> case v of
          Parser.Ast.Function args ret native -> pure $ Right ret
          x -> pure $ Left $ "Variable of type '" <> pack (show x) <> "' is not callable"
        Analyser.Util.Function vdt _ _ _ -> pure $ Right vdt
        Analyser.Util.Argument vdt -> undefined -- TODO
        Analyser.Util.IncompleteFunction args vdt native -> pure $ Right vdt
  ArbitraryBlock exprs -> getTypeOfExpr (Prelude.last exprs) gd
  -- semCheckExprs will bail out if type of ift /= type of iff
  Conditional cond ift iff -> getTypeOfExpr ift gd
  VariableDef {} -> pure $ Right NilType
  FunctionDef {} -> pure $ Right NilType
  AnonymousFunction {} -> undefined -- TODO
  Root {} -> pure $ Left "Unexpected root"
  Unary _ ex -> case ex of
    IntLiteral n -> pure $ Right Int
    FloatLiteral x -> pure $ Right Float
    _ -> pure $ Left "Unary operator applied to unexpected expression"