module Main where

import Analyser.Analyser (analyseAst, analyseAst')
import Analyser.Util as AU
  ( Def (Function),
    rFoldl,
    tfst,
    tsnd,
    tthd,
  )
import Control.Monad (void)
import Data.Bifunctor (Bifunctor (second))
import Data.HashMap.Strict as H (empty, fromList, union)
import Data.Text (Text, pack, unpack)
import Evaluator.Evaluator (evaluateExpression)
import Parser.Ast (VDataType (Float, Int, NilType, String))
import Parser.Parser (exprs, root)
import System.Console.Pretty
  ( Color (Green, Red, Yellow),
    Pretty (color, style),
    Style (Bold),
  )
import System.Environment.Blank (getArgs)
import System.IO (hPutStr, hPutStrLn, stderr, stdout)
import Text.Megaparsec (errorBundlePretty, parse)
import Text.Pretty.Simple (pPrint)

makeForeignFunction :: VDataType -> Def
makeForeignFunction ret = AU.Function ret [] [] True

logError :: String -> IO ()
logError toLog = do
  hPutStr stderr $ style Bold . color Red $ "[× fatal] "
  hPutStrLn stderr toLog

main' :: String -> IO ()
main' fileName = do
  contents <- readFile fileName
  let result = parse root fileName (pack contents)
  -- temporary, just a hack for now
  -- will be implemented properly when
  -- I have more time to work on it
  let stdlib =
        [ ("+i", Int),
          ("+f", Float),
          ("-i", Int),
          ("-f", Float),
          ("*i", Int),
          ("*f", Float),
          ("/i", Int),
          ("/f", Float),
          -- todo
          ("<", Float),
          ("<=", Float),
          (">", Float),
          (">=", Float),
          -- todo
          ("print", NilType),
          ("str", String)
        ]
  let defs = rFoldl stdlib H.empty $ \acc curr ->
        acc
          `H.union` H.fromList [second makeForeignFunction curr]
  case result of
    Left e -> putStrLn (errorBundlePretty e)
    Right res -> do
      let out = analyseAst res defs
      case tfst out of
        Left txt -> logError $ Data.Text.unpack txt
        Right ex -> void $ evaluateExpression (tsnd out) (tthd out) ex

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> putStrLn $ "usage: \n" <> "axl run program.axl"
    1 -> case head args of
      "run" -> main' "index.axl"
      "transpile" -> logError "transpilation is still wip"
      _ -> logError $ "unknown action '" <> head args <> "'"
    2 -> case head args of
      "run" -> main' $ last args
      "transpile" -> logError "transpilation is still wip"
      _ -> logError $ "unknown action '" <> head args <> "'"
    _ -> logError $ "expected at most 2 arguments, got " <> show (length args)
