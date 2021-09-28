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

{-
    In case of a FunctionDef, the things we need to check for are:

    * whether something with the same name has already been defined.
      this is not possible because redefinitions are not allowed.

    * whether the function return type is not explicitly defined but
      the return value is a call to the same function. this is not
      possible because it's impossible to determine the return type.

    * whether the regular analyseExprs check on all Exprs of the
      function body succeeds.

    * whether the actual return type of the function is the same as
      the expected return type in case the return type was explicitly
      defined by the user. In case it wasn't explicitly defined, the
      type of the value being returned is automatically the inferred
      return type of the function.
-}

-- type signature for the analyseExprs function
--
-- this could have been imported, but cyclic imports aren't very
-- good and it's just one value so I could very well pass it.
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
-- acc          :: [Either Text Expr]   -> the resultant accumulator for analyseExprs
-- analyseExprs :: AnalyseExprsFn       -> the analyseExprs function from Analyser.hs
-- name         :: Text                 -> the name of the function
-- vtype        :: VDataType            -> data type of the return value of the function
-- args         :: [(Text, VDataType)]  -> the arguments expected to be passed to the function
-- body         :: [Expr]               -> the Exprs that make up the function body; last expr is returned
-- native       :: Bool                 -> whether the function is a native function
analyseFunctionDef acc analyseExprs name vtype args body native = do
  env <- get

  -- lookup the global definition hashtable
  -- for an Expr with key as `name`.
  v <- liftIO $ H.lookup (fst env) name

  case v of
    -- if the key exists in the hashtable, send back an error
    -- telling the user redefinitions are not allowed.
    Just _ -> pure $ makeLeft $ "Redefinition of function " <> name
    -- if the key does not exist in the
    -- hashtable, we are good to go
    Nothing -> do
      h1 <- liftIO $ H.newSized 500

      -- There are two extra things we need to be able to refer to compared
      -- to the definitions previously made (global definitions -- fst env)
      --
      -- First is the function itself, since we should be able to make
      -- recursive calls. For this purpose we add an IncompleteFunction,
      -- which essentially is Function but without the body, since we
      -- don't need it here. At the end, this will be replaced by the
      -- proper Function Def, while this allows us to be able to use
      -- recursive calls without getting "undefined function" errors.
      --
      -- Second is the set of arguments that the function receives.
      -- TODO fix redef bug, arguments need to shadow prev defs.
      -- We just add the Argument Def to global defs here. During
      -- a function call, the Evaluator can union the global defs
      -- with the actual value that was passed during the call.
      --
      -- To add these two, we use hUnion' that adds all elements of
      -- a Haskell list to a hashtable.
      let v = [(name, IncompleteFunction args vtype native)] <> map (second Argument) args
      v' <- liftIO $ hUnion' v (fst env)

      -- We can now use analyseExprs to analyse all
      -- the Exprs in the function body.
      -- TODO fix function scope defs leaking out.
      result <-
        liftIO $
          runStateT
            (foldl analyseExprs (pure []) body)
            (v', h1)

      -- the Expr to be returned from the function
      let re = if null body then Nil else last body

      -- whether the return type was unspecified
      let inferred = vtype == Inferred

      -- we now add the definitions made inside of our function
      -- body to a hashtable v' and get the type of it in reType.
      fdefs <- liftIO $ (fst . snd) result `hUnion` fst env
      reType <- liftIO $ getTypeOfExpr re fdefs

      -- if the last Expr in the function body is a call to
      -- itself (a recursive call), we have no way of knowing
      -- what the return type should be, so make it a Left if
      -- that is the case.
      let reType' =
            if isFnCall name re && inferred
              then
                Left $
                  "cannot infer the return type of function '"
                    <> name
                    <> "' that returns a call to itself"
              else reType

      case reType' of
        -- If getTypeOfExpr failed on the last Expr
        -- or if last Expr was a recursive call to
        -- itself and no return type was specified,
        -- send back the error.
        Left err -> pure $ makeLeft err
        -- If everything went fine, we're good to go.
        Right dvdt -> do
          -- We can now add the Function as a Def to our global defs,
          -- overwriting the previous IncompleteFunction def.
          let fn = Analyser.Util.Function (if inferred then dvdt else vtype) args body native
          liftIO $ H.insert (fst env) name fn

          -- We also need to add the definitions made inside this
          -- function to the local defs (snd env). We use hUnion'
          -- for this purpose using a single element list.
          v' <- liftIO $ [(name, fn)] `hUnion'` (fst . snd) result
          liftIO $ H.insert (snd env) name v'

          -- we can now use `sequence` to convert out [Either Text Expr]
          -- into Either Text [Expr] and add the result to our accumulator.
          let res =
                acc
                  <> [ sequence (fst result) >>= \v ->
                         Right $ FunctionDef name dvdt args v native
                     ]
          -- check if the user explicitly defined a return type
          -- for this function, but returned a value of a different
          -- type.
          if not inferred
            then
              if vtype == dvdt
                then -- if the explicitly defined type
                -- is the type of the value being
                -- returned, just send back res.
                  pure res
                else -- send back a descriptive error
                -- if the return type is wrong

                  pure $
                    makeLeft $
                      "Expected function '"
                        <> name
                        <> "' to return "
                        <> (T.toLower . T.pack . show) vtype
                        <> ", instead got "
                        <> (T.toLower . T.pack . show) dvdt
            else -- if the return type was not explicitly defined,
            -- any type is fine since that is now the return
            -- type of the function, so just return `res`.
              pure res
