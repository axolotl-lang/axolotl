module Analyser.Analysers.ArbitraryBlock where

import Analyser.Util
import Control.Monad.State
import Data.Either.Combinators
import qualified Data.HashMap.Strict as H
import Data.Maybe
import qualified Data.Text as T
import Parser.Ast

type AnalyseExprsFn = State Env AnalyserResult -> Expr -> State Env AnalyserResult

analyseArbitraryBlock :: AnalyserResult -> [Expr] -> AnalyseExprsFn -> State Env AnalyserResult
analyseArbitraryBlock acc body analyseExprs = do
  env <- get
  let result = runState (foldl analyseExprs (pure []) body) (H.empty, H.empty)
  let r = getTypeOfExpr (if null body then Nil else last body) ((fst . snd) result `H.union` fst env)
  pure $ acc <> [sequence (fst result) >>= \v -> r >>= \t -> Right $ ArbitraryBlock v]
