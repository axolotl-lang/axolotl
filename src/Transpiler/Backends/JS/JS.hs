module Transpiler.Backends.JS.JS where

import qualified Analyser.Util as AU
import qualified Data.Text as T
import Parser.Ast

type IndentLevel = Int

type Backend = Expr -> AU.GDefs -> AU.LDefs -> IndentLevel -> T.Text

jsBackend :: Backend
jsBackend e@(Root exprs) gd ld il = undefined
