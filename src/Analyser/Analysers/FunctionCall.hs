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
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import Debug.Trace (trace)
import GHC.List (foldl')
import Parser.Ast (Expr, VDataType (Function))
import TextShow (TextShow (showt))

{-
    For a FunctionCall, the things we need to check for are:

    * whether the function being called exists.

    * whether the number of arguments being passed to the
      function in question is the same as the number of
      arguments expected by the function.

    * whether the arguments passed to the function conform
      to the expected type of the argument.
-}

-- makeVdtArr converts a list of Exprs into IO (Either Text [VDataType])
makeVdtArr :: Env -> [Expr] -> IO (Either T.Text [VDataType])
makeVdtArr env exprs = do
  v <- mapM (`getTypeOfExpr` fst env) exprs
  pure $ sequence v

rFoldl' list def fun = foldl' fun def list

-- checkArgs takes a list of expected VDataTypes and a list of actual VDataTypes,
-- and checks if the actual VDataType list is the same as the expected list.
checkArgs :: [VDataType] -> [VDataType] -> T.Text -> Bool -> (Bool, Int) -> Maybe T.Text
checkArgs expArgs actualArgs fnName variadic (recursiveCall, initialIndex) = do
  let arglen = length expArgs
  let argdiff = length actualArgs - length expArgs
  snd $
    -- rFoldl is just foldl but with the function coming later
    -- so I can use the lambda better.
    --
    -- Here, the accumulator is a tuple of (Int, Maybe Text),
    -- where the int is just used to keep track of the index,
    -- and the Maybe Text is actually an encountered error,
    -- which is Nothing if no error is found.
    rFoldl' (zip expArgs actualArgs) (1, Nothing) $ \acc curr ->
      ( fst acc + 1,
        if isJust (snd acc)
          then snd acc
          else
            if uncurry (==) curr
              then
                if fst acc == arglen && variadic && not recursiveCall
                  then checkArgs (map (const (fst curr)) [1 .. argdiff]) (drop arglen actualArgs) fnName True (True, arglen)
                  else Nothing
              else
                Just $
                  "Expected argument of type '"
                    <> (T.toLower . T.pack . show) (fst curr)
                    <> "' but got '"
                    <> (T.toLower . T.pack . show) (snd curr)
                    <> "' in argument "
                    <> showt (fst acc + (if recursiveCall then initialIndex else 0))
                    <> " of call to "
                    <> ( if variadic
                           then "variadic function "
                           else "function "
                       )
                    <> T.pack (show fnName)
      )

functionAnalyser ::
  AnalyserResult ->
  Expr ->
  T.Text ->
  [Expr] ->
  [VDataType] ->
  Bool ->
  Bool ->
  StateT Env IO AnalyserResult
functionAnalyser acc infExpr name args expArgs variadic native = do
  env <- get
  -- TODO check arguments to native functions when
  -- variable arguments are available.

  -- Here we check if the number of passed arguments
  -- is the same as the number of expected arguments.
  -- For now, we skip this check on native functions
  -- since variable arguments have not been implemented.
  if (if variadic then (>) else (/=)) (length expArgs) (length args) && not native
    then
      pure $
        makeLeft $
          "expected "
            <> (T.pack . show) (length expArgs)
            <> ( if variadic
                   then " or more"
                   else ""
               )
            <> " arguments, got "
            <> (T.pack . show) (length args)
            <> " in call to function '"
            <> name
            <> "'"
    else do
      -- we now need to check if the arguments conform to the
      -- expected types defined in the function definition.
      vdtArr <- liftIO $ makeVdtArr env args
      pure case vdtArr of
        Left err -> makeLeft err
        Right vdtArr' ->
          case checkArgs expArgs vdtArr' name variadic (False, 0) of
            Nothing -> acc <> [Right infExpr]
            Just txt -> makeLeft txt

analyseFunctionCall :: AnalyserResult -> Expr -> T.Text -> [Expr] -> StateT Env IO AnalyserResult
-- acc     :: [Either Text Expr]  -> the resultant accumulator for analyseExprs
-- infExpr :: Expr                -> the original FunctionCall Expr passed through replaceInferredVdt
-- name    :: Text                -> the function that is being called
-- args    :: [Expr]              -> the arguments passed to the function
analyseFunctionCall acc infExpr name args = do
  env <- get

  -- lookup name in global defs (gd)
  v <- liftIO $ H.lookup (fst env) name

  -- AnalyseExprs calls replaceInferredVdt for every expr in the
  -- expr array you give it initially, which in turn calls
  -- getTypeOfExpr in case of FunctionCall and VariableDef.
  -- Since we're at this point, it's safe to assume that v is Just.
  case fromJust v of
    --
    -- In case the user is trying to call a variable.
    Analyser.Util.Variable v _ -> case v of
      Parser.Ast.Function expArgs _ native variadic -> functionAnalyser acc infExpr name args expArgs variadic native
      x -> pure $ makeLeft $ "Variable of type '" <> T.pack (show x) <> "' is not callable"
    --
    -- In case the user is trying to call a Function.
    Analyser.Util.Function vdt expArgs _ native ->
      functionAnalyser acc infExpr name args (map snd (fst expArgs)) (snd expArgs) native
    --
    -- In case the user is trying to call an argument.
    Analyser.Util.Argument vdt -> {- TODO -} undefined
    --
    -- In case the user is trying to call an IncompleteFunction.
    -- This is not a problem, since this can happen during
    -- recursive calls.
    Analyser.Util.IncompleteFunction expArgs vtype native ->
      functionAnalyser acc infExpr name args (map snd (fst expArgs)) (snd expArgs) native