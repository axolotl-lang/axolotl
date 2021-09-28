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

{-
    For a FunctionCall, the things we need to check for are:

    * whether the function being called exists.

    * whether the number of arguments being passed to the
      function in question is the same as the number of
      arguments expected by the function.

    * whether the arguments passed to the function conform
      to the expected type of the argument.
-}

-- makeVdtArr converts a list of Exprs into IO (Either Text [VDataType]),
-- where a Left value is an error encountered in the getTypeOfExpr of
-- any element in the list.
makeVdtArr :: Env -> [Expr] -> IO (Either T.Text [VDataType])
makeVdtArr env exprs = do
  v <- mapM (`getTypeOfExpr` fst env) exprs
  pure $ sequence v

-- checkArgs takes a list of expected VDataTypes and a list of actual VDataTypes,
-- and checks if the actual VDataType list is the same as the expected list.
checkArgs :: [VDataType] -> [VDataType] -> T.Text -> Maybe T.Text
checkArgs expArgs actualArgs fnName = do
  snd $
    -- rFoldl is just foldl but with the function coming later
    -- so I can use the lambda better.
    --
    -- Here, the accumulator is a tuple of (Int, Maybe Text),
    -- where the int is just used to keep track of the index,
    -- and the Maybe Text is actually an encountered error,
    -- which is Nothing if no error is found.
    rFoldl (zip expArgs actualArgs) (0, Nothing) $ \acc curr ->
      ( fst acc + 1,
        if uncurry (==) curr
          then -- if there's no error, send back Nothing
            Nothing
          else -- if there's an error, send it back with Just

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

functionAnalyser ::
  AnalyserResult ->
  Expr ->
  T.Text ->
  [Expr] ->
  [VDataType] ->
  Bool ->
  StateT Env IO AnalyserResult
functionAnalyser acc infExpr name args expArgs native = do
  env <- get
  -- TODO check arguments to native functions when
  -- variable arguments are available.

  -- Here we check if the number of passed arguments
  -- is the same as the number of expected arguments.
  -- For now, we skip this check on native functions
  -- since variable arguments have not been implemented.
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
      -- if the number of arguments match, we now need
      -- to check if the arguments conform to the expected
      -- types defined in the function definition.
      vdtArr <- liftIO $ makeVdtArr env args
      pure case vdtArr of
        Left err -> makeLeft err
        Right vdtArr' ->
          case checkArgs expArgs vdtArr' name of
            Nothing -> acc <> [Right infExpr]
            Just txt -> makeLeft txt

analyseFunctionCall :: AnalyserResult -> Expr -> T.Text -> [Expr] -> StateT Env IO AnalyserResult
-- acc     :: [Either Text Expr]  -> the resultant accumulator for analyseExprs
-- infExpr :: Expr                -> the original FunctionCall Expr passed through replaceInferredVdt
-- name    :: Text                -> the function that is being called
-- args    :: [Expr]              -> the arguments passed to the function
analyseFunctionCall acc infExpr name args = do
  env <- get

  -- lookup the global definition hashtable
  -- for an Expr with key as `name`.
  v <- liftIO $ H.lookup (fst env) name

  -- AnalyseExprs calls replaceInferredVdt for every expr in the
  -- expr array you give it initially, which in turn calls
  -- getTypeOfExpr in case of FunctionCall and VariableDef.
  -- Since we're at this point, it's safe to assume that v is Just.
  case fromJust v of
    --
    -- In case the user is trying to call a variable.
    Analyser.Util.Variable v _ -> case v of
      Parser.Ast.Function expArgs _ native -> functionAnalyser acc infExpr name args expArgs native
      x -> pure $ makeLeft $ "Variable of type '" <> T.pack (show x) <> "' is not callable"
    --
    -- In case the user is trying to call a Function.
    Analyser.Util.Function vdt expArgs _ native ->
      functionAnalyser acc infExpr name args (map snd expArgs) native
    --
    -- In case the user is trying to call an argument.
    Analyser.Util.Argument vdt -> {- TODO -} undefined
    --
    -- In case the user is trying to call an IncompleteFunction.
    -- This is not a problem, since this can happen during
    -- recursive calls.
    Analyser.Util.IncompleteFunction expArgs vtype native ->
      functionAnalyser acc infExpr name args (map snd expArgs) native