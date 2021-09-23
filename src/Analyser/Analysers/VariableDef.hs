module Analyser.Analysers.VariableDef where

import Analyser.Util
  ( AnalyserResult,
    Def (Variable),
    Env,
    getTypeOfExpr,
    makeLeft,
  )
import Control.Monad.State (MonadState (get), StateT, modify)
import Data.Bifunctor (Bifunctor (first))
import qualified Data.HashMap.Strict as H
import Data.Text as T (Text, pack)
import Parser.Ast (Expr, VDataType (Inferred))

analyseVariableDef :: AnalyserResult -> Expr -> Text -> VDataType -> Expr -> StateT Env IO AnalyserResult
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
