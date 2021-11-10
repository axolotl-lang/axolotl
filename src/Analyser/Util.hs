module Analyser.Util where

import Data.Either.Combinators (maybeToLeft, maybeToRight)
import qualified Data.HashTable.IO as H
import Data.Text as T (Text, pack)
import Parser.Ast
  ( Expr (..),
    VDataType (ArrayOf, Bool, Float, Function, Inferred, Int, NilType, String),
  )

-- a Definition
data Def
  = Variable VDataType Expr
  | Function VDataType ([(Text, VDataType)], Bool) [Expr] Bool
  | Argument VDataType
  | -- this is used to allow recursive calls
    -- and to check if they are proper
    IncompleteFunction ([(Text, VDataType)], Bool) VDataType Bool
  deriving (Show, Eq)

-- global definitions - (text, Def)
type GDefs = H.BasicHashTable Text Def

-- local definitions - (text, GDefs)
type LDefs = H.BasicHashTable Text GDefs

type Env = (GDefs, LDefs)

type AnalyserResult = [Either Text Expr]

-- foldl but with the function coming later
-- this helps make the lambda more usable
rFoldl :: Foldable t => t a -> b -> (b -> a -> b) -> b
rFoldl list def fun = Prelude.foldl fun def list

-- creates an Left value in an array
makeLeft :: a -> [Either a b]
makeLeft r = [Left r]

getTypeFromArr :: VDataType -> VDataType
getTypeFromArr (ArrayOf x) = x
getTypeFromArr y = error "getTypeFromArr is only for ArrayOf"

-- adds all values from a into b
hUnion :: H.BasicHashTable Text Def -> H.BasicHashTable Text Def -> IO (H.BasicHashTable Text Def)
hUnion a b = H.mapM_ (uncurry (H.insert b)) a >> pure b

-- adds all values from a into b
hUnion' :: [(Text, Def)] -> H.BasicHashTable Text Def -> IO (H.BasicHashTable Text Def)
hUnion' a b = mapM_ (uncurry (H.insert b)) a >> pure b

-- checks if the expr is a FunctionCall calling name
isFnCall :: Text -> Expr -> Bool
isFnCall name' expr = case expr of
  FunctionCall name args -> name == name'
  _ -> False

-- get the VDataType of an Expr
getTypeOfExpr :: Expr -> GDefs -> IO (Either Text VDataType)
getTypeOfExpr ex gd = case ex of
  -- VDataType of Literal values is obvious
  IntLiteral {} -> pure $ Right Int
  FloatLiteral {} -> pure $ Right Float
  CharLiteral {} -> pure $ Right Int
  StrLiteral {} -> pure $ Right String
  BoolLiteral {} -> pure $ Right Bool
  Nil -> pure $ Right NilType
  -- definitions currently don't return anything
  VariableDef {} -> pure $ Right NilType
  FunctionDef {} -> pure $ Right NilType
  -- the type of an array is the type of the first item
  -- of the array.
  Array exs -> do
    getTypeOfExpr (Prelude.head exs) gd >>= \case
      Left txt -> pure $ Left txt
      Right vdt -> pure $ Right vdt
  --
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
        --
  FunctionCall name args -> do
    lu <- H.lookup gd name
    let def = maybeToRight ("call to undefined function '" <> name <> "'") lu
    case def of
      Left txt -> pure $ Left txt
      Right def' -> case def' of
        Analyser.Util.Variable v _ -> case v of
          Parser.Ast.Function args ret variadic native -> pure $ Right ret
          x -> pure $ Left $ "Variable of type '" <> pack (show x) <> "' is not callable"
        Analyser.Util.Function vdt _ _ _ -> pure $ Right vdt
        Analyser.Util.Argument vdt -> undefined -- TODO
        Analyser.Util.IncompleteFunction args vdt native -> do
          case vdt of
            Inferred -> do
              putStrLn "You're trying to call a recursive function without the manual definition of it's return type!"
              pure $ Right vdt
            _ -> pure $ Right vdt
  ArbitraryBlock exprs -> getTypeOfExpr (Prelude.last exprs) gd
  -- ideally we want type of ift to be the same as the type of iff
  -- but analyseExprs will bail out if type of ift /= type of iff
  -- so any is fine.
  Conditional cond ift iff -> getTypeOfExpr ift gd
  --
  Unary _ ex -> case ex of
    IntLiteral n -> pure $ Right Int
    FloatLiteral x -> pure $ Right Float
    _ -> pure $ Left "Unary operator applied to unexpected expression"
  --
  AnonymousFunction {} -> undefined -- TODO
  Root {} -> pure $ Left "Unexpected root"