module Evaluator.Evaluators.VariableUsage where

import Analyser.Util as AU
  ( Def (Function, Variable),
    GDefs,
    LDefs,
  )
import Data.HashTable.IO as H (lookup)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Parser.Ast
  ( Expr
      ( AnonymousFunction,
        Array,
        BoolLiteral,
        CharLiteral,
        FloatLiteral,
        FunctionCall,
        IntLiteral,
        Nil,
        StrLiteral
      ),
  )

type EvaluateExpressionFn = AU.GDefs -> AU.LDefs -> Expr -> IO Expr

analyseVariableUsage :: GDefs -> LDefs -> EvaluateExpressionFn -> T.Text -> IO Expr
analyseVariableUsage gd ld evaluateExpression x =
  H.lookup gd x >>= \v' -> case fromJust v' of
    AU.Variable _ expr -> case expr of
      IntLiteral n -> pure expr
      FloatLiteral y -> pure expr
      CharLiteral n -> pure expr
      StrLiteral txt -> pure expr
      BoolLiteral b -> pure expr
      Array exs -> pure expr
      Nil -> pure expr
      FunctionCall _ args -> evaluateExpression gd ld expr
      _ -> evaluateExpression gd ld expr
    AU.Function vdt args body native -> pure $ AnonymousFunction vdt args body
    -- argument values will be added as AU.Variable
    -- during the AnonymousFunction call
    _ -> error "should never happen"
