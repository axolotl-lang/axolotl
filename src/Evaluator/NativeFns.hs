module Evaluator.NativeFns where

import qualified Data.Text as T
import Parser.Ast
  ( Expr
      ( Array,
        BoolLiteral,
        CharLiteral,
        FloatLiteral,
        FunctionDef,
        IntLiteral,
        Nil,
        StrLiteral,
        VariableDef
      ),
    VDataType (Float, Int, NilType, String),
  )

getNumber :: Expr -> Double
getNumber e = case e of
  IntLiteral n -> fromIntegral n
  FloatLiteral x -> x
  CharLiteral c -> fromIntegral c
  x -> error $ "cannot create a number from " <> show x

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

getStr :: Expr -> T.Text
getStr x = case x of
  IntLiteral n -> tshow n
  FloatLiteral y -> tshow y
  CharLiteral n -> tshow [n]
  StrLiteral txt -> txt
  BoolLiteral b -> tshow b
  Array exs -> tshow exs
  Nil -> "nil"
  VariableDef txt vdt ex -> "nil"
  FunctionDef txt vdt x1 exs b -> "nil"
  y -> error $ "could not get string from " <> show y

evalNative :: T.Text -> VDataType -> [Expr] -> IO Expr
evalNative "+i" Int args = pure $ IntLiteral $ round (sum (map getNumber args))
evalNative "+f" Float args = pure $ FloatLiteral (sum (map getNumber args))
evalNative "-i" Int args = pure $ IntLiteral $ round (foldl1 (-) (map getNumber args))
evalNative "-f" Float args = pure $ FloatLiteral (foldl1 (-) (map getNumber args))
evalNative "*i" Int args = pure $ IntLiteral $ round (product (map getNumber args))
evalNative "*f" Float args = pure $ FloatLiteral (product (map getNumber args))
evalNative "/i" Int args = pure $ IntLiteral $ round (foldl1 (/) (map getNumber args))
evalNative "/f" Float args = pure $ FloatLiteral (foldl1 (/) (map getNumber args))
evalNative "str" String args = pure $ StrLiteral (foldl1 (<>) (map getStr args))
evalNative "print" NilType args = (putStrLn . T.unpack) (foldl1 (<>) (map getStr args)) >> pure Nil
evalNative name _ _ = error $ "evaluator does not know how to execute the native function " <> T.unpack name
