module Transpiler.Transpiler where

import qualified Analyser.Util as AU
import qualified Data.Text as T
import Parser.Ast (Expr (Root))

type IndentLevel = Int

type Backend = Expr -> IndentLevel -> T.Text

transpile :: Backend -> Expr -> IndentLevel -> T.Text
transpile b e@(Root exprs) il = b e il
transpile _ _ _ = error "transpile: must receive Root"
