module Analyser.Analysers.Conditional where

import Analyser.Util
import Control.Monad.State
import Data.Either.Combinators
import Data.Maybe
import Data.Text as T
import Parser.Ast

analyseConditional :: AnalyserResult -> Expr -> Expr -> Expr -> State Env AnalyserResult
analyseConditional acc cond ift iff = do
  env <- get
  case getTypeOfExpr cond (fst env) of
    Left txt -> pure $ makeLeft txt
    Right vdt -> case vdt of
      Bool -> do
        case getTypeOfExpr ift (fst env) of
          Left txt -> pure $ makeLeft txt
          Right t1 -> case getTypeOfExpr iff (fst env) of
            Left txt -> pure $ makeLeft txt
            Right t2 -> do
              if t1 == t2
                then pure $ acc <> [Right $ Conditional cond ift iff]
                else
                  pure $
                    makeLeft $
                      "expected both conditional branches to return the same type, instead got "
                        <> (T.pack . show) t1
                        <> " and "
                        <> (T.pack . show) t2
      _ -> pure $ makeLeft "condition in conditional doesn't return a bool"