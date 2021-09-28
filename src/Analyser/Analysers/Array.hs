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

{-
    In an Array, every element must conform to the VDataType
    of the Array, and getTypeOfExpr must succeed for all the
    Exprs in the Array.
-}

analyseArray :: AnalyserResult -> [Expr] -> Expr -> StateT Env IO AnalyserResult
-- acc   :: [Either Text Expr]  -> the resultant accumulator for analyseExprs
-- exprs :: [Expr]              -> the list of Exprs that an array is formed by
-- infExpr :: Expr              -> the original array Expr passed through replaceInferredVdt
analyseArray acc exprs infExpr = do
  env <- get
  v <- liftIO $ getTypeOfExpr infExpr (fst env)
  case v of
    Left err -> pure $ makeLeft err
    Right v -> do
      -- this is the data type that every Expr in the array
      -- should conform to.
      let expVdt = getTypeFromArr v

      -- create a new array with the (Either Text) VDataType of all exprs.
      exprTypes <- liftIO $ mapM (`getTypeOfExpr` fst env) exprs

      -- rFoldl is just a foldl but with the function
      -- coming later so I can use lambda properly.
      --
      -- We fold over exprTypes and return an error
      -- when a type doesn't conform to the expected type
      -- the accumulator is (currentIndex, Maybe (index, error)).
      let result = rFoldl exprTypes ((0, Nothing) :: (Int, Maybe Text)) $ \acc curr -> do
            -- utility function to send back acc more easily
            let ret x = (fst acc + 1, x)
            -- see if getTypeOfExpr for this Expr succeeded
            case curr of
              -- if it succeeded, see if the type
              -- is the same as the expected type
              -- for this array.
              Right vdt ->
                ret $
                  if vdt == expVdt
                    then Nothing
                    else
                      Just $
                        "Expected type "
                          <> (toLower . T.pack . show) expVdt
                          <> " but got "
                          <> (toLower . T.pack . show) vdt
                          <> " in index "
                          <> T.pack (show (fst acc))
                          <> " of array literal"
              -- getTypeOfExpr failed, send back the error
              Left err -> ret $ Just err
      case snd result of
        Nothing -> pure $ acc <> [Right infExpr]
        -- if we found an error, use makeLeft to send it back
        -- makeLeft is a utility function that essentially just
        -- creates errors.
        Just err -> pure $ makeLeft err
