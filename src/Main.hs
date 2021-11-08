{-# LANGUAGE TemplateHaskell #-}
module Main where

import Analyser.Analyser (analyseAst)
import Analyser.Util as AU
  ( Def (Function),
    rFoldl,
  )
import Control.Monad (void)
import Data.Bifunctor (Bifunctor (second))
import Data.HashTable.IO as H (fromListWithSizeHint)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Version (showVersion)
import Evaluator.Evaluator (evaluateExpression)
import Parser.Ast (VDataType (Bool, Float, Int, NilType, String, Any))
import Parser.Parser (exprs, root)
import Paths_axolotl (version)
import System.Console.Pretty
  ( Color (Red),
    Pretty (color, style),
    Style (Bold),
  )
import System.Environment.Blank (getArgs)
import System.IO (hPutStr, hPutStrLn, stderr, stdout)
import Text.Megaparsec (errorBundlePretty, parse)
import Text.Pretty.Simple (pPrint)
import Transpiler.Backends.JS.JS (jsBackend, jsStdlib)
import Transpiler.Transpiler (transpile)
import qualified Data.Text.Encoding as B
import Data.FileEmbed (embedFile)

axlStdlib :: T.Text
axlStdlib = B.decodeUtf8 $(embedFile "stdlib/math.axl")

makeNativeFunction :: VDataType -> Def
makeNativeFunction ret = AU.Function ret ([("args", Any)], True) [] True

logError :: String -> IO ()
logError toLog = do
  hPutStr stderr $ style Bold . color Red $ "[× fatal] "
  hPutStrLn stderr toLog

main' :: String -> Bool -> IO ()
main' fileName evaluate = do
  contents <- readFile fileName
  let result = parse root fileName (pack contents)
  -- temporary, just a hack for now
  -- will be implemented properly when
  -- I have more time to work on it
  case result of
    Left e -> putStrLn (errorBundlePretty e)
    Right res -> do
      v <- H.fromListWithSizeHint 1000 defs
      out <- analyseAst res v
      case fst out of
        Left txt -> logError $ unpack txt
        -- Right ex -> pPrint (tthd out) -- void $ evaluateExpression (tsnd out) (tthd out) ex
        Right ex ->
          if evaluate
            then void $ uncurry evaluateExpression (snd out) ex
            else putStrLn $ unpack $ jsStdlib <> transpile jsBackend ex 0

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> putStrLn $ "usage: \n" <> "axl run program.axl"
    1 -> case head args of
      "run" -> main' "index.axl" True
      "transpile" -> main' "index.axl" False
      "version" -> putStrLn $ showVersion version
      _ -> logError $ "unknown action '" <> head args <> "'"
    2 -> case head args of
      "run" -> main' (last args) True
      "transpile" -> main' (last args) False
      _ -> logError $ "unknown action '" <> head args <> "'"
    _ -> logError $ "expected at most 2 arguments, got " <> show (length args)
