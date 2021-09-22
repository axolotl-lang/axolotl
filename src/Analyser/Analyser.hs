module Analyser.Analyser where

import Analyser.Analysers.Array
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
  ( MonadState (get, put),
    State,
    modify,
    runState,
  )
import Data.Bifunctor (first, second)
import Data.Either.Combinators (fromLeft', fromRight, fromRight', isLeft, maybeToRight)
import Data.HashMap.Strict as H (HashMap, delete, empty, findWithDefault, fromList, insert, lookup, union)
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
makeDtArr :: Env -> [Expr] -> Either Text [VDataType]
makeDtArr acc = mapM (`getTypeOfExpr` fst acc)

checkArgs :: [VDataType] -> Either Text [VDataType] -> Text -> Maybe Text
checkArgs expArgs vdtArgs fnName = do
  case vdtArgs of
    Left txt -> Just txt
    Right vdts -> snd $
      rFoldl (zip expArgs vdts) (0, Nothing) $ \acc curr ->
        ( fst acc + 1,
          if uncurry (==) curr
            then Nothing
            else
              Just $
                "Expected argument of type '"
                  <> (toLower . T.pack . show) (fst curr)
                  <> "' but got '"
                  <> (toLower . T.pack . show) (snd curr)
                  <> "' in argument "
                  <> T.pack (show (fst acc + 1))
                  <> T.pack " of call to function "
                  <> T.pack (show fnName)
        )

analyseExprs :: (State Env AnalyserResult -> Expr -> State Env AnalyserResult)
analyseExprs acc' curr = do
  env <- get
  acc <- acc'
  if not (null acc) && isLeft (last acc)
    then put (H.empty, H.empty) >> pure [last acc]
    else case replaceInferredVdt curr (fst env) of
      Left err -> pure $ acc <> [Left err]
      Right infExpr -> case infExpr of
        Array exprs -> analyseArray acc exprs infExpr
        FunctionCall name args -> do
          -- since replaceInferredVdt evaluated to Right, this exists
          let def = fromJust $ H.lookup name (fst env)
          -- (def arg-1 arg-2 ...)
          case def of
            Analyser.Util.Variable v _ -> case v of
              Parser.Ast.Function expArgs _ ->
                -- TODO: remove this equality hack when variable args are available
                if (length expArgs /= length args) && (name /= "print") && (name /= "str")
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
                  else pure case checkArgs expArgs (makeDtArr env args) name of
                    Nothing -> acc <> [Right infExpr]
                    Just txt -> makeLeft txt
              x -> pure $ makeLeft $ "Variable of type '" <> pack (show x) <> "' is not callable"
            Analyser.Util.Function vdt expArgs _ frgn ->
              -- TODO: remove this equality hack when variable args are available
              if (length expArgs /= length args) && (name /= "print") && (name /= "str")
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
                else pure case checkArgs (map snd expArgs) (makeDtArr env args) name of
                  Nothing -> acc <> [Right infExpr]
                  Just txt -> makeLeft txt
            Analyser.Util.Argument vdt -> undefined -- TODO
            Analyser.Util.IncompleteFunction expArgs -> do
              -- TODO: remove this equality hack when variable args are available
              if (length expArgs /= length args) && (name /= "print") && (name /= "str")
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
                else pure case checkArgs (map snd expArgs) (makeDtArr env args) name of
                  Nothing -> acc <> [Right infExpr]
                  Just txt -> makeLeft txt
        VariableDef name vtype expr -> case H.lookup name (fst env) of
          Nothing -> do
            modify $ first (insert name (Analyser.Util.Variable vtype expr))
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
        Conditional cond ift iff -> do
          case getTypeOfExpr cond (fst env) of
            Left txt -> pure $ makeLeft txt
            Right vdt -> case vdt of
              Bool -> do
                case getTypeOfExpr ift (fst env) of
                  Left txt -> pure $ makeLeft txt
                  Right t1 -> case getTypeOfExpr iff (fst env) of
                    Left txt -> pure $ makeLeft txt
                    Right t2 -> do
                      if t1 == t2
                        then pure $ acc <> [Right $ Conditional cond ift iff]
                        else
                          pure $
                            makeLeft $
                              "expected both conditional branches to return the same type, instead got "
                                <> (T.pack . show) t1
                                <> " and "
                                <> (T.pack . show) t2
              _ -> pure $ makeLeft "condition in conditional doesn't return a bool"
        ArbitraryBlock body -> do
          let result = runState (foldl analyseExprs (pure []) body) (H.empty, H.empty)
          let r = getTypeOfExpr (if null body then Nil else last body) ((fst . snd) result `union` fst env)
          pure $ acc <> [sequence (fst result) >>= \v -> r >>= \t -> Right $ ArbitraryBlock v]
        FunctionDef name vtype args body frgn -> case H.lookup name (fst env) of
          Just _ -> pure $ makeLeft $ "Redefinition of function " <> name
          Nothing -> do
            let result =
                  runState
                    (foldl analyseExprs (pure []) body)
                    (fst env `union` H.fromList ([(name, IncompleteFunction args)] <> map (second Argument) args), H.empty)
            let lx = if null body then Nil else last body
            let inferred = vtype == Inferred
            let r =
                  if isFnCall name lx && inferred
                    then Left $ "cannot infer the return type of function '" <> name <> "' that returns a call to itself"
                    else getTypeOfExpr lx ((fst . snd) result `union` fst env)
            case r of
              Left txt -> pure $ makeLeft txt
              Right dvdt -> do
                let gdi = Analyser.Util.Function (if inferred then fromRight' r else vtype) args body frgn
                modify $ first (insert name gdi)
                modify $ second $ insert name (H.fromList [(name, gdi)] `H.union` (fst . snd) result)
                let res =
                      acc
                        <> [ sequence (fst result) >>= \v ->
                               r
                                 >>= \ct -> Right $ FunctionDef name ct args v frgn
                           ]
                if not inferred
                  then
                    if vtype == dvdt
                      then pure res
                      else
                        pure $
                          makeLeft $
                            "Expected function '"
                              <> name
                              <> "' to return "
                              <> (toLower . T.pack . show) vtype
                              <> ", instead got "
                              <> (toLower . T.pack . show) dvdt
                  else pure res
        _ -> pure $ acc <> [Right infExpr]

replaceInferredVdt :: Expr -> GDefs -> Either Text Expr
replaceInferredVdt (Root x) gd = error "fold with analyseExprs for this"
-- handle variable definition inside variable definition
replaceInferredVdt (VariableDef name x VariableDef {}) _ =
  Left "Cannot define a variable inside a variable"
-- infer types for proper variable definitions
replaceInferredVdt (VariableDef name Inferred y) gd =
  getTypeOfExpr y gd >>= \t -> Right $ VariableDef name t y
-- infer function call types
replaceInferredVdt (FunctionCall name args) gd =
  getTypeOfExpr (FunctionCall name args) gd >>= \t ->
    Right $ FunctionCall name args
-- send back nodes that don't need type inference
replaceInferredVdt x _ = Right x

analyseAst :: Expr -> GDefs -> (Either Text Expr, GDefs, LDefs)
analyseAst (Root x) gd = do
  let t = runState (foldl analyseExprs (pure []) x) (H.empty, H.empty)
  (sequence (fst t) >>= \v -> Right (Root v), (fst . snd) t, (snd . snd) t)
analyseAst _ _ = undefined

analyseAst' :: Expr -> Either Text Expr
analyseAst' (Root x) = do
  let t = runState (foldl analyseExprs (pure []) x) (H.empty, H.empty)
  sequence (fst t) >>= \v -> Right (Root v)
analyseAst' _ = undefined