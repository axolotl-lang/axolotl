module Evaluator.NativeFns where

import Data.List (intersperse)
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
    VDataType (Bool, Float, Int, NilType, String),
  )
import TextShow (TextShow (showt))

getNumber :: Expr -> Double
getNumber e = case e of
  IntLiteral n -> fromIntegral n
  FloatLiteral x -> x
  CharLiteral c -> fromIntegral c
  x -> error $ "cannot create a number from " <> show x

getStr :: Expr -> T.Text
getStr x = case x of
  IntLiteral n -> showt n
  FloatLiteral y -> showt y
  CharLiteral n -> showt [n]
  StrLiteral txt -> txt
  BoolLiteral b -> showt b
  Array exs -> "[" <> foldl (<>) "" (intersperse "," (map showt exs)) <> "]"
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
evalNative ">" Bool exprs =
  if length exprs == 2
    then case (head exprs, exprs !! 1) of
      (IntLiteral iv, IntLiteral iv') -> pure $ BoolLiteral (iv > iv')
      _ -> undefined -- TODO
    else error "can only equality check two arguments"
evalNative name _ _ = error $ "evaluator does not know how to execute the native function '" <> T.unpack name <> "'"
