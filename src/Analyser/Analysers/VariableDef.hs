module Analyser.Analysers.VariableDef where

import Analyser.Util
  ( AnalyserResult,
    Def (Variable),
    Env,
    getTypeOfExpr,
    makeLeft,
  )
import Control.Monad.State (MonadState (get), StateT, modify)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Bifunctor (Bifunctor (first))
import Data.Either.Combinators (fromRight')
import qualified Data.HashTable.IO as H
import Data.Maybe (fromJust)
import Data.Text as T (Text, pack)
import Parser.Ast (Expr, VDataType (Inferred))

{-
    In case of a variable definition, we need to make
    sure that the variable is not being redefined, and
    that the value being assigned to the variable is
    of the type of the variable.

-}

analyseVariableDef :: AnalyserResult -> Expr -> Text -> VDataType -> Expr -> StateT Env IO AnalyserResult
-- acc   :: [Either Text Expr]  -> the resultant accumulator for analyseExprs
-- infExpr :: Expr              -> the original variable def Expr passed through replaceInferredVdt
-- name  :: Text                -> the name of the variable
-- vtype :: VDataType           -> data type of the variable
-- expr  :: Expr                -> value contained in the variable
analyseVariableDef acc infExpr name vtype expr = do
  env <- get

  -- lookup name in global defs (gd)
  v <- liftIO $ H.lookup (fst env) name

  case v of
    -- if the key does not exist in the
    -- hashtable, we are good to go
    Nothing -> do
      -- insert the variable into the global definitions hashtable
      liftIO $ H.insert (fst env) name (Analyser.Util.Variable vtype expr)

      -- type of the expr that is being assigned
      atype <- liftIO $ getTypeOfExpr expr (fst env)

      -- Analyser.hs calls replaceInferredVdt on
      -- every Expr that it comes across which in
      -- case of a VariableDef, runs the Expr that is
      -- being assigned to the variable through getTypeOfExpr,
      -- so we can assume it's Right since we're here.
      let actualVtype = fromRight' atype

      -- see if the Expr being assigned
      -- is of the type of the variable
      if actualVtype == vtype
        then -- if it is, just add it to the resultant accumulator
          pure $ acc <> [Right infExpr]
        else
          pure $
            makeLeft $
              "Cannot assign value of type "
                <> T.pack (show actualVtype)
                <> " to variable of type "
                <> T.pack (show vtype)

    -- key exists in the hashtable
    Just _ -> pure [Left $ "Redefinition of variable " <> name]
