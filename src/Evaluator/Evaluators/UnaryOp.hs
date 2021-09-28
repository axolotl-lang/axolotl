module Evaluator.Evaluators.UnaryOp where

import Parser.Ast (Expr (FloatLiteral, IntLiteral), UnaryOp)

analyseUnaryOp :: UnaryOp -> Expr -> IO Expr
analyseUnaryOp op expr = case expr of
  -- right now there's only one unary operator
  -- but when there's more, we'll need to pattern
  -- match over `op` as well
  IntLiteral n -> pure $ IntLiteral $ -1 * n
  FloatLiteral x -> pure $ FloatLiteral $ -1 * x
  _ -> error "Unary operator applied to unexpected argument"
