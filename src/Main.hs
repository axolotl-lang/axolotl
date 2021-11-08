{-# LANGUAGE TemplateHaskell #-}

module Main where

import Analyser.Analyser (analyseAst)
import Analyser.Util as AU
  ( Def (Function),
    hUnion,
    rFoldl,
  )
import Control.Monad (void)
import Control.Monad.Except (ExceptT, MonadError (throwError), MonadIO (liftIO), liftEither, runExceptT)
import Data.Bifunctor (Bifunctor (second))
import Data.FileEmbed (embedFile)
import Data.HashTable.IO as H (delete, fromListWithSizeHint, mapM_)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as B
import Data.Version (showVersion)
import Data.Void (Void)
import Evaluator.Evaluator (evaluateExpression)
import Parser.Ast (VDataType (Any, Bool, Float, Int, NilType, String))
import Parser.Parser (exprs, root)
import Paths_axolotl (version)
import System.Console.Pretty
  ( Color (Red),
    Pretty (color, style),
    Style (Bold),
  )
import System.Environment.Blank (getArgs)
import System.IO (hPutStr, hPutStrLn, stderr, stdout)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, parse)
import Text.Pretty.Simple (pPrint)
import Transpiler.Backends.JS.JS (jsBackend, jsStdlib)
import Transpiler.Transpiler (transpile)

axlStdlib :: T.Text
axlStdlib = B.decodeUtf8 $(embedFile "stdlib/stdlib.axl")

makeNativeFunction :: VDataType -> Def
makeNativeFunction ret = AU.Function ret ([("args", Any)], True) [] True

logError :: String -> IO ()
logError toLog = do
  hPutStr stderr $ style Bold . color Red $ "[× fatal] "
  hPutStrLn stderr toLog

stringLeft :: Either (ParseErrorBundle Text Void) b -> Either String b
stringLeft (Left x) = Left $ errorBundlePretty x
stringLeft (Right y) = Right y

stringLeft' :: Either Text b -> Either String b
stringLeft' (Left x) = Left $ unpack x
stringLeft' (Right y) = Right y

main' :: String -> Bool -> ExceptT String IO ()
main' fileName evaluate = do
  -- get defs from stdlib
  resultStdlib <- liftEither $ stringLeft $ parse root fileName axlStdlib
  v <- liftIO $ H.fromListWithSizeHint 1000 []
  outStdlib <- liftIO $ analyseAst resultStdlib v

  -- prepare to run program
  contents <- liftIO $ readFile fileName
  result <- liftEither $ stringLeft $ parse root fileName (pack contents)
  liftIO $ H.delete v "args"
  out <- liftIO $ analyseAst result v
  ex <- liftEither $ stringLeft' $ fst out
  if evaluate
    then liftIO $ void $ uncurry evaluateExpression (snd out) ex
    else throwError $ unpack $ jsStdlib <> transpile jsBackend ex 0

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> putStrLn $ "usage: \n" <> "axl run program.axl"
    1 -> case head args of
      "run" -> do
        val <- runExceptT $ main' "index.axl" True
        case val of
          Left err -> logError err
          Right v -> pure v
      "transpile" -> do
        val <- runExceptT $ main' "index.axl" False
        case val of
          Left err -> logError err
          Right v -> pure v
      "version" -> putStrLn $ showVersion version
      _ -> logError $ "unknown action '" <> head args <> "'"
    2 -> case head args of
      "run" -> do
        val <- runExceptT $ main' (last args) True
        case val of
          Left err -> logError err
          Right v -> pure v
      "transpile" -> do
        val <- runExceptT $ main' (last args) False
        case val of
          Left err -> logError err
          Right v -> pure v
      _ -> logError $ "unknown action '" <> head args <> "'"
    _ -> logError $ "expected at most 2 arguments, got " <> show (length args)
