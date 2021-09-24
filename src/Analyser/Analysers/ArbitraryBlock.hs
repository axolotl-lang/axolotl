module Analyser.Analysers.ArbitraryBlock where

import Analyser.Util (AnalyserResult, Env, getTypeOfExpr, hUnion)
import Control.Monad.State
  ( MonadIO (liftIO),
    MonadState (get),
    StateT (runStateT),
  )
import qualified Data.HashTable.IO as H
import qualified Data.Text as T
import Parser.Ast (Expr (ArbitraryBlock, Nil))

type AnalyseExprsFn = StateT Env IO AnalyserResult -> Expr -> StateT Env IO AnalyserResult

analyseArbitraryBlock :: AnalyserResult -> [Expr] -> AnalyseExprsFn -> StateT Env IO AnalyserResult
analyseArbitraryBlock acc body analyseExprs = do
  env <- get
  h1 <- liftIO $ H.newSized 5000
  h2 <- liftIO $ H.newSized 5000
  let result = runStateT (foldl analyseExprs (pure []) body) (h1, h2)
  result <- liftIO result
  r <- liftIO $ getTypeOfExpr (if null body then Nil else last body) ((fst . snd) result `hUnion` fst env)
  pure $ acc <> [sequence (fst result) >>= \v -> r >>= \t -> Right $ ArbitraryBlock v]
