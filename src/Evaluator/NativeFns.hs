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

evalNative :: (T.Text, VDataType) -> [Expr] -> IO Expr
evalNative nt args
  | nt == ("+i", Int) = pure $ IntLiteral $ round (sum (map getNumber args))
  | nt == ("+f", Float) = pure $ FloatLiteral (sum (map getNumber args))
  | nt == ("-i", Int) = pure $ IntLiteral $ round (foldl1 (-) (map getNumber args))
  | nt == ("-f", Float) = pure $ FloatLiteral (foldl1 (-) (map getNumber args))
  | nt == ("*i", Int) = pure $ IntLiteral $ round (product (map getNumber args))
  | nt == ("*f", Float) = pure $ FloatLiteral (product (map getNumber args))
  | nt == ("/i", Int) = pure $ IntLiteral $ round (foldl1 (/) (map getNumber args))
  | nt == ("/f", Float) = pure $ FloatLiteral (foldl1 (/) (map getNumber args))
  | nt == ("str", String) = pure $ StrLiteral (foldl1 (<>) (map getStr args))
  | nt == ("print", NilType) = (putStrLn . T.unpack) (foldl1 (<>) (map getStr args)) >> pure Nil
  | nt == (">", Bool)
      || nt == (">=", Bool)
      || nt == ("<", Bool)
      || nt == ("<=", Bool)
      || nt == ("/=", Bool)
      || nt == ("==", Bool) =
    if length args == 2
      then case (head args, args !! 1) of
        (IntLiteral iv, IntLiteral iv') ->
          pure $
            BoolLiteral
              ( ( case fst nt of
                    ">" -> (>)
                    ">=" -> (>=)
                    "<" -> (<)
                    "<=" -> (<=)
                    "==" -> (==)
                    "!=" -> (/=)
                )
                  iv
                  iv'
              )
        (FloatLiteral iv, IntLiteral iv') -> pure $ BoolLiteral (iv > fromIntegral iv')
        (IntLiteral iv, FloatLiteral iv') -> pure $ BoolLiteral (fromIntegral iv > iv')
        (FloatLiteral iv, FloatLiteral iv') -> pure $ BoolLiteral (iv > iv')
        (x, y) -> error $ "Can't compare " <> show x <> " with " <> show y
      else error "can only equality check two arguments"
  | otherwise = error $ "evaluator does not know how to execute the native function '" <> T.unpack (fst nt) <> "'"
