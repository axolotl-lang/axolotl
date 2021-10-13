module Transpiler.Util where

import Data.Foldable (foldlM)
import Data.List (intersperse)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T

repeatText :: T.Text -> Int -> T.Text
repeatText t n = foldl (<>) "" (map (const t) [1 .. n])

makeCommaSep :: T.Text -> [T.Text] -> T.Text -> T.Text
makeCommaSep header exprs footer =
  header
    <> foldl (<>) "" (intersperse ", " exprs)
    <> footer