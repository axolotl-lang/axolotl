module Analyser.Analysers.Conditional where

import Analyser.Util
  ( AnalyserResult,
    Env,
    getTypeOfExpr,
    makeLeft,
  )
import Control.Monad.State (MonadState (get), StateT)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Either.Combinators (fromRight')
import Data.Text as T (pack)
import Parser.Ast (Expr (Conditional), VDataType (Bool))

{-
    In case of a conditional, we need to check
    if the condition Expr returns a Bool, and
    that both branches of the conditional return
    a value of the same data type.
-}

analyseConditional :: AnalyserResult -> Expr -> Expr -> Expr -> StateT Env IO AnalyserResult
-- acc   :: [Either Text Expr]  -> the resultant accumulator for analyseExprs
-- cond  :: Expr -> the condition to evaluate, must return bool
-- ift   :: Expr -> the Expr to return if cond is **true**
-- iff   :: Expr -> the Expr to return if cond is **false**
analyseConditional acc cond ift iff = do
  env <- get

  -- get the data type of the condition Expr
  v <- liftIO $ getTypeOfExpr cond (fst env)

  case v of
    Left txt -> pure $ makeLeft txt
    Right vdt -> do
      -- check if condition Expr is Bool
      case vdt of
        -- if the condition Expr returns Bool,
        -- we now need to check if both branches
        -- return a value of the same type.
        Bool -> do
          vdtIft <- liftIO $ getTypeOfExpr ift (fst env)
          case vdtIft of
            -- getTypeOfExpr on ift failed
            Left err -> pure $ makeLeft err
            Right t1 -> do
              vdtIff <- liftIO $ getTypeOfExpr iff (fst env)
              case vdtIff of
                -- getTypeOfExpr on iff failed
                Left err -> pure $ makeLeft err
                Right t2 -> do
                  -- see if both branches return same type
                  if t1 == t2
                    then -- add to resultant accumulator
                      pure $ acc <> [Right $ Conditional cond ift iff]
                    else
                      pure $
                        makeLeft $
                          "expected both conditional branches to return the same type, instead got "
                            <> (T.pack . show) t1
                            <> " and "
                            <> (T.pack . show) t2
        -- if the condition Expr is not of the data type Bool, send
        -- back a descriptive error informing the user of the same.
        _ -> pure $ makeLeft "condition in conditional doesn't return a bool"