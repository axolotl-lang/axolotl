module Analyser.Analysers.FunctionDef where

import Analyser.Util
  ( AnalyserResult,
    Def (Argument, Function, IncompleteFunction),
    Env,
    getTypeOfExpr,
    hUnion,
    hUnion',
    isFnCall,
    makeLeft,
  )
import Control.Monad.State
  ( MonadIO (liftIO),
    MonadState (get),
    StateT (runStateT),
    modify,
  )
import Data.Bifunctor (Bifunctor (first, second))
import Data.Either.Combinators (fromRight')
import qualified Data.HashTable.IO as H
import qualified Data.Text as T
import Debug.Trace (trace)
import Parser.Ast (Expr (FunctionDef, Nil), VDataType (Inferred))

type AnalyseExprsFn = StateT Env IO AnalyserResult -> Expr -> StateT Env IO AnalyserResult

analyseFunctionDef ::
  AnalyserResult ->
  AnalyseExprsFn ->
  T.Text ->
  VDataType ->
  [(T.Text, VDataType)] ->
  [Expr] ->
  Bool ->
  StateT Env IO AnalyserResult
analyseFunctionDef acc analyseExprs name vtype args body frgn = do
  env <- get
  v <- liftIO $ H.lookup (fst env) name
  case v of
    Just _ -> pure $ makeLeft $ "Redefinition of function " <> name
    Nothing -> do
      h1 <- liftIO $ H.newSized 500
      let v = [(name, IncompleteFunction args vtype)] <> map (second Argument) args
      v' <- liftIO $ hUnion' v (fst env)
      result <-
        liftIO $
          runStateT
            (foldl analyseExprs (pure []) body)
            (v', h1)
      let lx = if null body then Nil else last body
      let inferred = vtype == Inferred
      v' <- liftIO $ (fst . snd) result `hUnion` fst env
      v <- liftIO $ getTypeOfExpr lx v'
      let r =
            if isFnCall name lx && inferred
              then Left $ "cannot infer the return type of function '" <> name <> "' that returns a call to itself"
              else v
      case r of
        Left txt -> pure $ makeLeft txt
        Right dvdt -> do
          let gdi = Analyser.Util.Function (if inferred then fromRight' r else vtype) args body frgn
          let v = [(name, gdi)]
          v' <- liftIO $ v `hUnion'` (fst . snd) result
          liftIO $ H.insert (fst env) name gdi
          liftIO $ H.insert (snd env) name v'
          let res =
                acc
                  <> [ sequence (fst result) >>= \v ->
                         r
                           >>= \ct -> Right $ FunctionDef name ct args v frgn
                     ]
          if not inferred
            then
              if vtype == dvdt
                then pure res
                else
                  pure $
                    makeLeft $
                      "Expected function '"
                        <> name
                        <> "' to return "
                        <> (T.toLower . T.pack . show) vtype
                        <> ", instead got "
                        <> (T.toLower . T.pack . show) dvdt
            else pure res
