module Interpreter
    ( interpret
    ) where

import Ast
import Symtab
import Control.Monad

eval :: SymTab -> AstNode -> IO (AstNode, SymTab)
eval s n = return (Nil, mempty)

interpret :: Ast -> IO (AstNode, SymTab)
interpret (Ast nodes) = foldM (eval . snd) (Nil, mempty) nodes
