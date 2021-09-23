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
    tfst,
    tsnd,
    tthd,
  )
import Control.Monad.State
import Data.Bifunctor (first, second)
import Data.Either.Combinators (fromLeft', fromRight, fromRight', isLeft, maybeToRight)
import Data.HashMap.Strict as H (HashMap, delete, empty, findWithDefault, fromList, insert, lookup, union)
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

  globalDefs here is a hashmap - (DefName, Expr), simple enough
  localDefs is a hashmap - (scopeName, hashmap (DefName, Expr))

  so if you have two functions called a and b, and you defined a
  variable age=20 in a and name="udit" in b, you'll have localDefs as
  [
    [ "a", [ ("age", IntLiteral 20) ] ]
    [ "b", [ ("name", StringLiteral "udit") ] ]
  ]

  inferredTree is the expr array you passed it with all Inferred
  in it's tree replaced with actual types inferred from context

  analyseExprs calls replaceInferredVdt for every expr in the expr array you give it
  initially, which in turn in most cases calls getTypeOfExpr

  I use analyseExprs for AST Root (just array of all expr in program)
  and function bodies here, but it can be used anywhere you want to
  infer types and analyse a set of expressions
-}

{-
  Cases where a type-check is necessary:
  * variable definition when the type is explicitly defined
  * function definition when the return type is explicitly defined
  * function call (whether all arguments confirm to needed types)
  * array generation (whether all arguments confirm to needed type)

  note that if the type of an array is explicitly defined, every
  element in the array must have the same type, and in case the
  type is _not_ explicitly defined, every element in the array
  must have the same type as the first element in the array
-}

analyseExprs :: (StateT Env IO AnalyserResult -> Expr -> StateT Env IO AnalyserResult)
analyseExprs acc' curr = do
  env <- get
  acc <- acc'
  v <- liftIO $ replaceInferredVdt curr (fst env)
  case v of
    Left err -> pure $ acc <> [Left err]
    Right infExpr -> case infExpr of
      Array exprs -> analyseArray acc exprs infExpr
      FunctionCall name args -> analyseFunctionCall acc infExpr name args
      VariableDef name vtype expr -> analyseVariableDef acc infExpr name vtype expr
      Conditional cond ift iff -> analyseConditional acc cond ift iff
      ArbitraryBlock body -> analyseArbitraryBlock acc body analyseExprs
      FunctionDef name vtype args body frgn -> analyseFunctionDef acc analyseExprs name vtype args body frgn
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

analyseAst :: Expr -> GDefs -> IO (Either Text Expr, GDefs, LDefs)
analyseAst (Root x) gd = do
  h <- H.newSized 5000
  t <- runStateT (foldl analyseExprs (pure []) x) (gd, h)
  pure (sequence (fst t) >>= \v -> Right (Root v), (fst . snd) t, (snd . snd) t)
analyseAst _ _ = undefined