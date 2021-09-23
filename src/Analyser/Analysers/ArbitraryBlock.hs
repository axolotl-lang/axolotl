module Analyser.Analysers.ArbitraryBlock where

import Analyser.Util (AnalyserResult, Env, getTypeOfExpr)
import Control.Monad.State
  ( MonadIO (liftIO),
    MonadState (get),
    StateT (runStateT),
  )
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Parser.Ast (Expr (ArbitraryBlock, Nil))

type AnalyseExprsFn = StateT Env IO AnalyserResult -> Expr -> StateT Env IO AnalyserResult

analyseArbitraryBlock :: AnalyserResult -> [Expr] -> AnalyseExprsFn -> StateT Env IO AnalyserResult
analyseArbitraryBlock acc body analyseExprs = do
  env <- get
  result <- liftIO $ runStateT (foldl analyseExprs (pure []) body) (H.empty, H.empty)
  let r = getTypeOfExpr (if null body then Nil else last body) ((fst . snd) result `H.union` fst env)
  pure $ acc <> [sequence (fst result) >>= \v -> r >>= \t -> Right $ ArbitraryBlock v]
