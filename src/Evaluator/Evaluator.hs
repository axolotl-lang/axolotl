module Evaluator.Evaluator where

import qualified Analyser.Util as AU
import Data.HashMap.Strict as H (fromList, lookup, union)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Evaluator.NativeFns (evalNative)
import Parser.Ast as PA (Expr (..))

-- note that you do not ever need to directly use
-- the local definitions, they are only to be passed
-- to nested calls as their global definitions

evaluateExpression :: AU.GDefs -> AU.LDefs -> Expr -> IO Expr
-- evaluate all items in root
evaluateExpression gd ld (Root exprs) = mapM (evaluateExpression gd ld) exprs >>= \y -> pure $ last y
-- replace with the value of the variable
evaluateExpression gd ld (VariableUsage x) = case fromJust (H.lookup x gd) of
  AU.Variable _ expr -> case expr of
    IntLiteral n -> pure expr
    FloatLiteral y -> pure expr
    CharLiteral n -> pure expr
    StrLiteral txt -> pure expr
    BoolLiteral b -> pure expr
    Array exs -> pure expr
    Nil -> pure expr
    FunctionCall _ args -> evaluateExpression gd ld expr
    _ -> trace (show expr) (evaluateExpression gd ld expr)
  -- VariableDef txt vdt ex -> pure Nil
  -- FunctionDef txt vdt x1 exs b -> pure Nil
  -- Root exs -> error "unexpected root"
  -- _ -> evaluateExpression gd ld expr
  AU.Function vdt args body frgn -> pure $ AnonymousFunction vdt args body
  -- argument values will be added as AU.Variable
  -- during the AnonymousFunction call
  AU.Argument _ -> error "should never happen"
  AU.IncompleteFunction args -> undefined -- TODO
  -- replace with the result of application of unary operator
evaluateExpression gd ld (Unary op expr) = case expr of
  -- right now there's only one unary operator
  -- but when there's more, we'll need to pattern
  -- match over `op` as well
  IntLiteral n -> pure $ IntLiteral $ -1 * n
  FloatLiteral x -> pure $ FloatLiteral $ -1 * x
  _ -> error "Unary operator applied to unexpected argument"
-- replace with the result of function call
evaluateExpression gd ld (FunctionCall name argExprs) = do
  let fun = fromJust (H.lookup name gd)
  let defs = fromJust (H.lookup name ld)
  case fun of
    AU.Variable vdt ex -> undefined -- TODO
    AU.Function vdt args exprs False -> do
      argExprs' <- mapM (evaluateExpression gd ld) argExprs
      let unionArgs = zipWith (\d a -> (fst a, AU.Variable (snd a) d)) argExprs' args
      evaluateExpression (H.fromList unionArgs `H.union` defs) ld $ AnonymousFunction vdt args exprs
    -- this is a native function
    AU.Function vdt _ _ True -> do
      evArgs <- Prelude.mapM (evaluateExpression gd ld) argExprs
      evalNative name vdt evArgs
    AU.Argument vdt -> undefined -- TODO
    AU.IncompleteFunction args -> undefined -- TODO
evaluateExpression gd ld (Conditional cond ift iff) = do
  cond' <- evaluateExpression gd ld cond
  case cond' of
    BoolLiteral x -> evaluateExpression gd ld (if x then ift else iff)
    _ -> error "condition returned non-bool value during evaluation"
evaluateExpression gd ld (AnonymousFunction ret args body) = evaluateExpression gd ld (Root body)
evaluateExpression gd ld (ArbitraryBlock body) = evaluateExpression gd ld (Root body)
evaluateExpression gd ld VariableDef {} = pure Nil
evaluateExpression gd ld FunctionDef {} = pure Nil
evaluateExpression gd ld r = pure r
