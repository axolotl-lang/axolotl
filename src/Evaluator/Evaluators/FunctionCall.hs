module Evaluator.Evaluators.FunctionCall where

import Analyser.Util as AU
  ( Def (Function, Variable),
    GDefs,
    LDefs,
    hUnion',
    rFoldl,
  )
import Data.HashTable.IO as H (lookup)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Parser.Ast (Expr (AnonymousFunction, Array), VDataType)

type EvaluateExpressionFn = AU.GDefs -> AU.LDefs -> Expr -> IO Expr

type EvalNativeFn = T.Text -> VDataType -> [Expr] -> IO Expr

rFoldl1 :: Foldable t => t a -> (a -> a -> a) -> a
rFoldl1 list fun = foldl1 fun list

evaluateFunctionCall ::
  AU.GDefs ->
  AU.LDefs ->
  T.Text ->
  EvaluateExpressionFn ->
  [Expr] ->
  EvalNativeFn ->
  IO Expr
evaluateFunctionCall gd ld name evaluateExpression argExprs evalNative = do
  fun <- H.lookup gd name
  defs <- H.lookup ld name

  case fromJust fun of
    AU.Variable vdt ex -> undefined -- TODO

    -- this is a function
    AU.Function vdt args exprs False -> do
      argExprs' <- mapM (evaluateExpression gd ld) argExprs
      let args' = fst args
      let variadic = snd args
      let arglen = length args'

      let unionArgs =
            zipWith3
              ( \i d a -> do
                  ( fst a,
                    AU.Variable
                      (snd a)
                      if i == arglen && variadic
                        then Array (drop (arglen - 1) argExprs')
                        else d
                    )
              )
              [1 ..]
              argExprs'
              args'

      v' <- unionArgs `hUnion'` fromJust defs
      evaluateExpression v' ld $ AnonymousFunction vdt args exprs

    -- this is a native function
    AU.Function vdt _ _ True -> do
      evArgs <- Prelude.mapM (evaluateExpression gd ld) argExprs
      evalNative name vdt evArgs
    _ -> error "should never happen"
