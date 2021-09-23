module Analyser.Analysers.Array where

import Analyser.Util
  ( AnalyserResult,
    Env,
    getTypeFromArr,
    getTypeOfExpr,
    makeLeft,
    rFoldl,
  )
import Control.Monad.State (MonadState (get), StateT)
import Data.Either.Combinators (fromRight')
import Data.Maybe (isJust)
import Data.Text as T (pack, toLower)
import Parser.Ast (Expr)

analyseArray :: AnalyserResult -> [Expr] -> Expr -> StateT Env IO AnalyserResult
analyseArray acc exprs infExpr = do
  env <- get
  -- since replaceInferredVdt evaluated to Right, this exists
  let at = getTypeFromArr $ fromRight' $ getTypeOfExpr infExpr (fst env)
  let mapped = mapM (`getTypeOfExpr` fst env) exprs
  case mapped of
    Left txt -> pure $ makeLeft txt
    Right mvdts -> do
      let res = rFoldl exprs (0, Nothing) $ \get' curr -> do
            let et = getTypeOfExpr curr (fst env)
            let ni = fst get' + 1
            if isJust (snd get')
              then (ni, snd get')
              else case et of
                Left txt -> (ni, Just txt)
                Right avdt ->
                  if avdt == at
                    then (ni, Nothing)
                    else
                      ( ni,
                        Just $
                          "Expected type '"
                            <> (toLower . T.pack . show) at
                            <> "' but got '"
                            <> (toLower . T.pack . show) avdt
                            <> "' in index "
                            <> T.pack (show (fst get'))
                            <> " of array literal"
                      )
      pure $ maybe (acc <> [Right infExpr]) makeLeft (snd res)
