module Evaluator.Evaluator where

import qualified Analyser.Util as AU
import Data.Maybe (fromJust)
import Evaluator.Evaluators.FunctionCall (evaluateFunctionCall)
import Evaluator.Evaluators.UnaryOp (analyseUnaryOp)
import Evaluator.Evaluators.VariableUsage (analyseVariableUsage)
import Evaluator.NativeFns (evalNative)
import Parser.Ast as PA (Expr (..))

-- note that you do not ever need to directly use
-- the local definitions, they are only to be passed
-- to nested calls as their global definitions

evaluateExpression :: AU.GDefs -> AU.LDefs -> Expr -> IO Expr
-- evaluate all items in root
evaluateExpression gd ld (Root exprs) =
  mapM (evaluateExpression gd ld) exprs >>= \y -> pure $ last y
-- replace with the value of the variable
evaluateExpression gd ld (VariableUsage name) =
  analyseVariableUsage gd ld evaluateExpression name
-- replace with the result of application of unary operator
evaluateExpression gd ld (Unary op expr) =
  analyseUnaryOp op expr
-- replace with the result of function call
evaluateExpression gd ld (FunctionCall name argExprs) =
  evaluateFunctionCall gd ld name evaluateExpression argExprs evalNative
-- replace with the if-true (ift) or if-false (iff) expr
evaluateExpression gd ld (Conditional cond ift iff) = do
  evaluateExpression gd ld cond >>= \case
    BoolLiteral x -> evaluateExpression gd ld (if x then ift else iff)
    _ -> error "condition returned non-bool value during evaluation"
-- replace with the result of the anonymous function call
evaluateExpression gd ld (AnonymousFunction ret args body) = evaluateExpression gd ld (Root body)
-- replace with the result of the last expr of the block
evaluateExpression gd ld (ArbitraryBlock body) = evaluateExpression gd ld (Root body)
-- replace with Nil
evaluateExpression gd ld VariableDef {} = pure Nil
-- replace with Nil
evaluateExpression gd ld FunctionDef {} = pure Nil
-- this leaves only literal values, so just send it back as is
evaluateExpression gd ld r = pure r
