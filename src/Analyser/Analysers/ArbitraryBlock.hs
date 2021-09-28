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

{-
    Analysing arbitrary blocks means we need to analyse
    all the expressions inside that arbitrary block, so
    we just run it thorugh analyseExprs.
-}

type AnalyseExprsFn = StateT Env IO AnalyserResult -> Expr -> StateT Env IO AnalyserResult

analyseArbitraryBlock :: AnalyserResult -> [Expr] -> AnalyseExprsFn -> StateT Env IO AnalyserResult
-- acc   :: [Either Text Expr]    -> the resultant accumulator for analyseExprs
-- body  :: [Expr]                -> the set of exprs that the block is formed by
-- analyseExprs :: AnalyseExprsFn -> the analyseExprs function from Analyser.hs
analyseArbitraryBlock acc body analyseExprs = do
  env <- get
  h1 <- liftIO $ H.newSized 1000
  h2 <- liftIO $ H.newSized 1000
  result <- liftIO $ runStateT (foldl analyseExprs (pure []) body) (h1, h2)
  -- we now need to union the definitions made inside
  -- this scope with the definitions made outside this scope
  -- before this point, and pass them to getTypeOfExpr, since
  -- the previously made definitions can be required to
  -- determine the type of an expression in the current
  -- scope, and so they must be available too
  let env' = snd result
  gd <- liftIO $ fst env' `hUnion` fst env
  r <- liftIO $ getTypeOfExpr (if null body then Nil else last body) gd

  pure $ acc <> [sequence (fst result) >>= \v -> r >> Right (ArbitraryBlock v)]
