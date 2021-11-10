module Sugar.Sugar where

import Analyser.Util (rFoldl)
import qualified Data.Text as T
import Debug.Trace (trace)
import Parser.Ast (Expr (FloatLiteral, FunctionCall))

mFilter :: [a] -> (a -> Bool) -> [a]
mFilter a b = filter b a

doesHaveFloat :: [Expr] -> Bool
doesHaveFloat exprs =
  not $
    null
      ( mFilter
          exprs
          ( \a -> do
              case a of
                FloatLiteral _ -> True
                _ -> False
          )
      )

desugarExpr :: Expr -> Expr
desugarExpr e@(FunctionCall name args) = do
  let mkFc =
        if doesHaveFloat args
          then FunctionCall (name <> "f") args
          else FunctionCall (name <> "i") args

  case name of
    "+" -> mkFc
    "-" -> mkFc
    "*" -> mkFc
    "/" -> FunctionCall "/f" args
    _ -> e
desugarExpr x = x