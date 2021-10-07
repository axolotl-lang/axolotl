module Transpiler.Util where

import Data.Foldable (foldlM)
import Data.Maybe (fromJust)
import qualified Data.Text as T

repeatText :: T.Text -> Int -> T.Text
repeatText t n = foldl (<>) "" (map (const t) [1 .. n])

makeCommaSep :: T.Text -> [T.Text] -> T.Text -> T.Text
makeCommaSep header exprs footer =
  header
    <> snd
      ( fromJust
          ( T.uncons
              ( foldl
                  (\acc curr -> acc <> ", " <> curr)
                  " "
                  exprs
              )
          )
      )
    <> footer