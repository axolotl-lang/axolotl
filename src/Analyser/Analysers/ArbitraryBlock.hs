module Analyser.Analysers.ArbitraryBlock where

import Analyser.Util (AnalyserResult, Env, getTypeOfExpr)
import Control.Monad.State (MonadState (get), State, runState)
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Parser.Ast (Expr (ArbitraryBlock, Nil))

type AnalyseExprsFn = State Env AnalyserResult -> Expr -> State Env AnalyserResult

analyseArbitraryBlock :: AnalyserResult -> [Expr] -> AnalyseExprsFn -> State Env AnalyserResult
analyseArbitraryBlock acc body analyseExprs = do
  env <- get
  let result = runState (foldl analyseExprs (pure []) body) (H.empty, H.empty)
  let r = getTypeOfExpr (if null body then Nil else last body) ((fst . snd) result `H.union` fst env)
  pure $ acc <> [sequence (fst result) >>= \v -> r >>= \t -> Right $ ArbitraryBlock v]
