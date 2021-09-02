module Analyser.Analyser where

import Analyser.Util
  ( Def (Argument, Function, Variable),
    GDefs,
    LDefs,
    getTypeFromArr,
    getTypeFromExpr,
    rFoldl,
    tfst,
    tsnd,
    tthd,
  )
import Data.Bifunctor (second)
import Data.Either.Combinators (fromLeft', fromRight, fromRight', isLeft, maybeToRight)
import Data.HashMap.Strict as H (HashMap, empty, findWithDefault, fromList, insert, lookup, union)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Text as T (Text, empty, pack, toLower, unpack)
import Debug.Trace (trace)
import Parser.Ast
  ( Expr (Array, FunctionCall, FunctionDef, Nil, Root, Variable, VariableDef),
    VDataType (Function, Inferred, NilType),
  )

{-
  semCheckExprs should be used to fold over a list of expressions,
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

  semCheckExprs calls inferType for every expr in the expr array you give it
  initially, which in turn in most cases calls getTypeFromExpr

  I use semCheckExprs for AST Root (just array of all expr in program)
  and function bodies here, but it can be used anywhere you want to
  infer types and analyse a set of expressions
-}
type Accumulator = (GDefs, LDefs, [Either Text Expr])

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
makeDtArr :: Accumulator -> [Expr] -> Either Text [VDataType]
makeDtArr acc = mapM (`getTypeFromExpr` tfst acc)

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

semCheckExprs :: (Accumulator -> Expr -> Accumulator)
semCheckExprs acc curr = do
  let makeLeft r = (H.empty, H.empty, [Left r])
  if not (null (tthd acc)) && isLeft (last (tthd acc))
    then (H.empty, H.empty, [last (tthd acc)])
    else case inferType curr (tfst acc) of
      Left err -> (tfst acc, tsnd acc, tthd acc <> [Left err])
      -- if it's a def, add to a1 or a2, else just add expr to a3
      Right infExpr -> case infExpr of
        Array exprs -> do
          -- since inferType evaluated to Right, this exists
          let at = getTypeFromArr $ fromRight' $ getTypeFromExpr infExpr (tfst acc)
          let mapped = mapM (`getTypeFromExpr` tfst acc) exprs
          case mapped of
            Left txt -> makeLeft txt
            Right mvdts -> do
              let res = rFoldl exprs (0, Nothing) $ \acc' curr -> do
                    let et = getTypeFromExpr curr (tfst acc)
                    let ni = fst acc' + 1
                    if isJust (snd acc')
                      then (ni, snd acc')
                      else case et of
                        Left txt -> (ni, Just txt)
                        Right avdt ->
                          if avdt == at
                            then (ni, Nothing)
                            else
                              ( ni,
                                Just $
                                  "Expected type '"
                                    <> (toLower . T.pack . show) at
                                    <> "' but got '"
                                    <> (toLower . T.pack . show) avdt
                                    <> "' in index "
                                    <> T.pack (show (fst acc'))
                                    <> " of array literal"
                              )
              maybe (tfst acc, tsnd acc, tthd acc <> [Right infExpr]) makeLeft (snd res)
        FunctionCall name args -> do
          -- since inferType evaluated to Right, this exists
          let def = fromJust $ H.lookup name (tfst acc)
          -- (def arg-1 arg-2 ...)
          case def of
            Analyser.Util.Variable v _ -> case v of
              Parser.Ast.Function expArgs _ ->
                maybe
                  (tfst acc, tsnd acc, tthd acc <> [Right infExpr])
                  makeLeft
                  (checkArgs expArgs (makeDtArr acc args) name)
              x -> makeLeft $ "Variable of type '" <> pack (show x) <> "' is not callable"
            Analyser.Util.Function vdt expArgs _ frgn ->
              maybe
                (tfst acc, tsnd acc, tthd acc <> [Right infExpr])
                makeLeft
                (checkArgs (map snd expArgs) (makeDtArr acc args) name)
            Analyser.Util.Argument vdt -> undefined -- TODO
        VariableDef name vtype expr -> case H.lookup name (tfst acc) of
          Nothing -> do
            let res =
                  ( insert name (Analyser.Util.Variable vtype expr) (tfst acc),
                    tsnd acc,
                    tthd acc <> [Right infExpr]
                  )
            if vtype /= Inferred
              then do
                let atype = getTypeFromExpr expr (tfst acc)
                case atype of
                  Left txt -> makeLeft txt
                  Right vdt ->
                    if vdt == vtype
                      then res
                      else
                        makeLeft $
                          "Cannot assign value of type "
                            <> T.pack (show vdt)
                            <> " to variable of type "
                            <> T.pack (show vtype)
              else res
          Just _ -> (H.empty, H.empty, [Left $ "Redefinition of variable " <> name])
        FunctionDef name vtype args body frgn -> case H.lookup name (tfst acc) of
          Just _ -> (H.empty, H.empty, [Left $ "Redefinition of function " <> name])
          Nothing -> do
            let result =
                  foldl
                    semCheckExprs
                    (tfst acc `union` H.fromList (map (second Argument) args), H.empty, [])
                    body
            let r =
                  getTypeFromExpr
                    (if null body then Nil else last body)
                    (tfst result `union` tfst acc)
            let inferred = vtype == Inferred
            case r of
              Left txt -> makeLeft txt
              Right dvdt -> do
                let res =
                      ( insert name (Analyser.Util.Function (if inferred then fromRight' r else vtype) args body frgn) (tfst acc),
                        insert name (tfst result) (tsnd acc),
                        tthd acc
                          <> [ sequence (tthd result) >>= \v ->
                                 r
                                   >>= \ct -> Right $ FunctionDef name ct args v frgn
                             ]
                      )
                if not inferred
                  then
                    if vtype == dvdt
                      then res
                      else
                        makeLeft $
                          "Expected function '"
                            <> name
                            <> "' to return "
                            <> (toLower . T.pack . show) vtype
                            <> ", instead got "
                            <> (toLower . T.pack . show) dvdt
                  else res
        _ -> (tfst acc, tsnd acc, tthd acc <> [Right infExpr])

inferType :: Expr -> GDefs -> Either Text Expr
inferType (Root x) gd = error "fold with semCheckExprs for this"
-- handle variable definition inside variable definition
inferType (VariableDef name x VariableDef {}) _ =
  Left "Cannot define a variable inside a variable"
-- infer types for proper variable definitions
inferType (VariableDef name Inferred y) gd =
  getTypeFromExpr y gd >>= \t -> Right $ VariableDef name t y
-- infer function call types
inferType (FunctionCall name args) gd =
  getTypeFromExpr (FunctionCall name args) gd >>= \t ->
    Right $ FunctionCall name args
-- send back nodes that don't need type inference
inferType x _ = Right x

analyseAst :: Expr -> GDefs -> (Either Text Expr, GDefs, LDefs)
analyseAst (Root x) gd = do
  let t = foldl semCheckExprs (gd, H.empty, []) x
  (sequence (tthd t) >>= \v -> Right (Root v), tfst t, tsnd t)
analyseAst _ _ = undefined

analyseAst' :: Expr -> Either Text Expr
analyseAst' (Root x) = do
  let t = foldl semCheckExprs (H.empty, H.empty, []) x
  sequence (tthd t) >>= \v -> Right (Root v)
analyseAst' _ = undefined