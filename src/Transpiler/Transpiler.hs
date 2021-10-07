module Transpiler.Transpiler where

import qualified Analyser.Util as AU
import qualified Data.Text as T
import Parser.Ast (Expr (Root))

type IndentLevel = Int

type Backend = Expr -> AU.GDefs -> AU.LDefs -> IndentLevel -> T.Text

transpile :: Backend -> Expr -> AU.GDefs -> AU.LDefs -> IndentLevel -> T.Text
transpile b e@(Root exprs) gd ld il = b e gd ld il
transpile _ _ _ _ _ = error "transpile: must receive Root"
