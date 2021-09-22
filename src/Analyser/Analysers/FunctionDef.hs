module Analyser.Analysers.FunctionDef where

import Analyser.Util
import Control.Monad.State
import Data.Bifunctor
import Data.Either.Combinators
import qualified Data.HashMap.Strict as H
import Data.Maybe
import qualified Data.Text as T
import Parser.Ast

type AnalyseExprsFn = State Env AnalyserResult -> Expr -> State Env AnalyserResult

analyseFunctionDef ::
  AnalyserResult ->
  AnalyseExprsFn ->
  T.Text ->
  VDataType ->
  [(T.Text, VDataType)] ->
  [Expr] ->
  Bool ->
  State Env AnalyserResult
analyseFunctionDef acc analyseExprs name vtype args body frgn = do
  env <- get
  case H.lookup name (fst env) of
    Just _ -> pure $ makeLeft $ "Redefinition of function " <> name
    Nothing -> do
      let result =
            runState
              (foldl analyseExprs (pure []) body)
              (fst env `H.union` H.fromList ([(name, IncompleteFunction args)] <> map (second Argument) args), H.empty)
      let lx = if null body then Nil else last body
      let inferred = vtype == Inferred
      let r =
            if isFnCall name lx && inferred
              then Left $ "cannot infer the return type of function '" <> name <> "' that returns a call to itself"
              else getTypeOfExpr lx ((fst . snd) result `H.union` fst env)
      case r of
        Left txt -> pure $ makeLeft txt
        Right dvdt -> do
          let gdi = Analyser.Util.Function (if inferred then fromRight' r else vtype) args body frgn
          modify $ first (H.insert name gdi)
          modify $ second $ H.insert name (H.fromList [(name, gdi)] `H.union` (fst . snd) result)
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
