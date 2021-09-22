module Analyser.Analysers.VariableDef where

import Analyser.Util
import Control.Monad.State
import Data.Bifunctor
import Data.Either.Combinators
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Text as T
import Parser.Ast

analyseVariableDef :: AnalyserResult -> Expr -> Text -> VDataType -> Expr -> State Env AnalyserResult
analyseVariableDef acc infExpr name vtype expr = do
  env <- get
  case H.lookup name (fst env) of
    Nothing -> do
      modify $ first (H.insert name (Analyser.Util.Variable vtype expr))
      let res = acc <> [Right infExpr]
      if vtype /= Inferred
        then do
          let atype = getTypeOfExpr expr (fst env)
          case atype of
            Left txt -> pure $ makeLeft txt
            Right vdt ->
              if vdt == vtype
                then pure res
                else
                  pure $
                    makeLeft $
                      "Cannot assign value of type "
                        <> T.pack (show vdt)
                        <> " to variable of type "
                        <> T.pack (show vtype)
        else pure res
    Just _ -> pure [Left $ "Redefinition of variable " <> name]
