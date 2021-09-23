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
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Either.Combinators (fromRight')
import Data.Maybe (isJust)
import Data.Text as T (Text, pack, toLower)
import Parser.Ast (Expr)

analyseArray :: AnalyserResult -> [Expr] -> Expr -> StateT Env IO AnalyserResult
analyseArray acc exprs infExpr = do
  env <- get
  v <- liftIO $ getTypeOfExpr infExpr (fst env)
  -- since replaceInferredVdt evaluated to Right, this exists
  let at = getTypeFromArr $ fromRight' v
  mapped <- liftIO $ mapM (`getTypeOfExpr` fst env) exprs
  case sequence mapped of
    Left txt -> pure $ makeLeft txt
    Right mvdts -> do
      let res = rFoldl exprs (pure (0, Nothing) :: IO (Int, Maybe Text)) $ \get'' curr -> do
            et <- liftIO $ getTypeOfExpr curr (fst env)
            get' <- get''
            let ni = fst get' + 1
            if isJust (snd get')
              then pure (ni, snd get')
              else case et of
                Left txt -> pure (ni, Just txt)
                Right avdt ->
                  if avdt == at
                    then pure (ni, Nothing)
                    else
                      pure
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
      res' <- liftIO res
      case snd res' of
        Nothing -> pure $ acc <> [Right infExpr]
        Just txt -> pure $ makeLeft txt