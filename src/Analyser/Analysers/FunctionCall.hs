module Analyser.Analysers.FunctionCall where

import Analyser.Util
  ( AnalyserResult,
    Def (Argument, Function, IncompleteFunction, Variable),
    Env,
    getTypeOfExpr,
    makeLeft,
    rFoldl,
  )
import Control.Monad.State (MonadIO (liftIO), MonadState (get), StateT)
import qualified Data.HashTable.IO as H
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Parser.Ast (Expr, VDataType (Function))

makeDtArr :: Env -> [Expr] -> IO (Either T.Text [VDataType])
makeDtArr acc exprs = do
  v <- mapM (`getTypeOfExpr` fst acc) exprs
  pure $ sequence v

checkArgs :: [VDataType] -> Either T.Text [VDataType] -> T.Text -> Maybe T.Text
checkArgs expArgs vdtArgs fnName = do
  case vdtArgs of
    Left txt -> Just txt
    Right vdts -> snd $
      rFoldl (zip expArgs vdts) (0, Nothing) $ \acc curr ->
        ( fst acc + 1,
          if uncurry (==) curr
            then Nothing
            else
              Just $
                "Expected argument of type '"
                  <> (T.toLower . T.pack . show) (fst curr)
                  <> "' but got '"
                  <> (T.toLower . T.pack . show) (snd curr)
                  <> "' in argument "
                  <> T.pack (show (fst acc + 1))
                  <> T.pack " of call to function "
                  <> T.pack (show fnName)
        )

analyseFunctionCall :: AnalyserResult -> Expr -> T.Text -> [Expr] -> StateT Env IO AnalyserResult
analyseFunctionCall acc infExpr name args = do
  env <- get
  v <- liftIO $ H.lookup (fst env) name
  -- since replaceInferredVdt evaluated to Right, this exists
  let def = fromJust v
  -- (def arg-1 arg-2 ...)
  case def of
    Analyser.Util.Variable v _ -> case v of
      Parser.Ast.Function expArgs _ native ->
        -- TODO: remove this equality hack when variable args are available
        if (length expArgs /= length args) && not native
          then
            pure $
              makeLeft $
                "expected "
                  <> (T.pack . show) (length expArgs)
                  <> " arguments, got "
                  <> (T.pack . show) (length args)
                  <> " in call to function '"
                  <> name
                  <> "'"
          else do
            v <- liftIO $ makeDtArr env args
            pure case checkArgs expArgs v name of
              Nothing -> acc <> [Right infExpr]
              Just txt -> makeLeft txt
      x -> pure $ makeLeft $ "Variable of type '" <> T.pack (show x) <> "' is not callable"
    Analyser.Util.Function vdt expArgs _ native ->
      -- TODO: remove this equality hack when variable args are available
      if (length expArgs /= length args) && not native
        then
          pure $
            makeLeft $
              "expected "
                <> (T.pack . show) (length expArgs)
                <> " arguments, got "
                <> (T.pack . show) (length args)
                <> " in call to function '"
                <> name
                <> "'"
        else do
          v <- liftIO $ makeDtArr env args
          pure case checkArgs (map snd expArgs) v name of
            Nothing -> acc <> [Right infExpr]
            Just txt -> makeLeft txt
    Analyser.Util.Argument vdt -> undefined -- TODO
    Analyser.Util.IncompleteFunction expArgs -> do
      -- TODO: remove this equality hack when variable args are available
      if (length expArgs /= length args) && (name /= "print") && (name /= "str")
        then
          pure $
            makeLeft $
              "expected "
                <> (T.pack . show) (length expArgs)
                <> " arguments, got "
                <> (T.pack . show) (length args)
                <> " in call to function '"
                <> name
                <> "'"
        else do
          v <- liftIO $ makeDtArr env args
          pure case checkArgs (map snd expArgs) v name of
            Nothing -> acc <> [Right infExpr]
            Just txt -> makeLeft txt