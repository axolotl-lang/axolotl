module Analyser.Analyser where

import Analyser.Analysers.ArbitraryBlock (analyseArbitraryBlock)
import Analyser.Analysers.Array (analyseArray)
import Analyser.Analysers.Conditional (analyseConditional)
import Analyser.Analysers.FunctionCall (analyseFunctionCall)
import Analyser.Analysers.FunctionDef (analyseFunctionDef)
import Analyser.Analysers.VariableDef (analyseVariableDef)
import Analyser.Util
  ( AnalyserResult,
    Def (Argument, Function, IncompleteFunction, Variable),
    Env,
    GDefs,
    LDefs,
    getTypeFromArr,
    getTypeOfExpr,
    isFnCall,
    makeLeft,
    rFoldl,
  )
import Control.Monad.State
  ( MonadIO (liftIO),
    MonadState (get),
    StateT (runStateT),
  )
import Data.Bifunctor (first, second)
import Data.Either.Combinators (fromLeft', fromRight, fromRight', isLeft, maybeToRight)
import qualified Data.HashTable.IO as H
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Text as T (Text, empty, pack, toLower, unpack)
import Debug.Trace (trace)
import Parser.Ast
  ( Expr (ArbitraryBlock, Array, Conditional, FunctionCall, FunctionDef, Nil, Root, VariableDef),
    VDataType (Bool, Function, Inferred, NilType),
  )

{-
  analyseExprs should be used to fold over a list of expressions,
  with the final result being (globalDefs, localDefs, inferredTree)

  globalDefs here is a hashtable - (DefName, Expr), simple enough
  localDefs is a hashtable - (scopeName, hashtable (DefName, Expr))

  so if you have two functions called a and b, and you defined a
  variable age=20 in a and name="udit" in b, you'll have localDefs as
  [
    [ "a", [ ("age", IntLiteral 20) ] ]
    [ "b", [ ("name", StringLiteral "udit") ] ]
  ]

  inferredTree is the expr array you passed it with all Inferred
  in it's tree replaced with actual types inferred from context.

  analyseExprs calls replaceInferredVdt for every expr in the
  expr array you give it initially, which in turn calls
  getTypeOfExpr in case of FunctionCall and VariableDef.

  I use analyseExprs for AST Root (just array of all expr in program)
  and function bodies here, but it can be used anywhere you want to
  infer types and analyse a set of expressions.
-}

analyseExprs :: (StateT Env IO AnalyserResult -> Expr -> StateT Env IO AnalyserResult)
analyseExprs acc' curr = do
  env <- get
  acc <- acc'
  -- infer types, replacing all Inferred in the AST with actual types
  v <- liftIO $ replaceInferredVdt curr (fst env)
  case v of
    Left err -> pure $ acc <> [Left err]
    Right infExpr -> case infExpr of
      -- exprs :: [Expr]  -> the list of Exprs that an array is formed by
      Array exprs -> analyseArray acc exprs infExpr
      -- name  :: Text    -> the function that is being called
      -- args  :: [Expr]  -> the arguments passed to the function
      FunctionCall name args -> analyseFunctionCall acc infExpr name args
      -- name  :: Text      -> the name of the variable
      -- vtype :: VDataType -> data type of the variable
      -- expr  :: Expr      -> value contained in the variable
      VariableDef name vtype expr -> analyseVariableDef acc infExpr name vtype expr
      -- cond  :: Expr -> the condition to evaluate, must return bool
      -- ift   :: Expr -> the Expr to return if cond is **true**
      -- iff   :: Expr -> the Expr to return if cond is **false**
      Conditional cond ift iff -> analyseConditional acc cond ift iff
      -- body  :: [Expr] -> the set of exprs that the block is formed by
      ArbitraryBlock body -> analyseArbitraryBlock acc body analyseExprs
      -- name   :: Text                -> the name of the function
      -- vtype  :: VDataType           -> data type of the return value of the function
      -- args   :: [(Text, VDataType)] -> the arguments expected to be passed to the function
      -- body   :: [Expr]              -> the Exprs that make up the function body; last expr is returned
      -- native :: Bool                -> whether the function is a native function
      FunctionDef name vtype args body native -> analyseFunctionDef acc analyseExprs name vtype args body native
      _ -> pure $ acc <> [Right infExpr]

replaceInferredVdt :: Expr -> GDefs -> IO (Either Text Expr)
replaceInferredVdt (Root x) gd = error "fold with analyseExprs for this"
-- handle variable definition inside variable definition
replaceInferredVdt (VariableDef name x VariableDef {}) _ =
  pure $ Left "Cannot define a variable inside a variable"
-- infer types for proper variable definitions
replaceInferredVdt (VariableDef name Inferred y) gd = do
  t' <- getTypeOfExpr y gd
  case t' of
    Left txt -> pure $ Left txt
    Right t -> pure $ Right $ VariableDef name t y
-- infer function call types
replaceInferredVdt (FunctionCall name args) gd =
  getTypeOfExpr (FunctionCall name args) gd >>= \t ->
    pure $ Right $ FunctionCall name args
-- send back nodes that don't need type inference
replaceInferredVdt x _ = pure $ Right x

analyseAst :: Expr -> GDefs -> IO (Either Text Expr, (GDefs, LDefs))
analyseAst (Root x) gd = do
  h <- H.newSized 1000
  t <- runStateT (foldl analyseExprs (pure []) x) (gd, h)
  pure (sequence (fst t) >>= \v -> Right (Root v), snd t)
analyseAst _ _ = undefined