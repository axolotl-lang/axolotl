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

    Note that Analyser.hs calls replaceInferredVdt on
    every Expr that it comes across (including this one)
    which in case of a VariableDef, runs the Expr that is
    being assigned to the variable through getTypeOfExpr.

    This means that we can safely assume that getTypeOfExpr
    on the Expr being assigned will surely be Just.
-}

analyseVariableDef :: AnalyserResult -> Expr -> Text -> VDataType -> Expr -> StateT Env IO AnalyserResult
-- acc   :: [Either Text Expr]  -> the resultant accumulator for analyseExprs
-- infExpr :: Expr              -> the original variable def Expr passed through replaceInferredVdt
-- name  :: Text                -> the name of the variable
-- vtype :: VDataType           -> data type of the variable
-- expr  :: Expr                -> value contained in the variable
analyseVariableDef acc infExpr name vtype expr = do
  env <- get

  -- lookup the global definition hashtable
  -- for an Expr with key as `name`.
  v <- liftIO $ H.lookup (fst env) name

  case v of
    -- if the key does not exist in the
    -- hashtable, we are good to go
    Nothing -> do
      -- insert the variable into the global definitions hashtable
      liftIO $ H.insert (fst env) name (Analyser.Util.Variable vtype expr)

      -- get the type of the expr that is being
      -- assigned to the variable name.
      atype <- liftIO $ getTypeOfExpr expr (fst env)

      -- fromRight' here is safe as described
      -- in the comment at the top of this file
      let actualVtype = fromRight' atype

      -- see if the Expr being assigned
      -- is of the type of the variable
      if actualVtype == vtype
        then -- if it is, just add it to the resultant accumulator
          pure $ acc <> [Right infExpr]
        else -- otherwise send back an error

          pure $
            makeLeft $
              "Cannot assign value of type "
                <> T.pack (show actualVtype)
                <> " to variable of type "
                <> T.pack (show vtype)

    -- if the key exists in the hashtable, send back an error
    -- telling the user redefinitions are not allowed.
    Just _ -> pure [Left $ "Redefinition of variable " <> name]
