module Interpreter
    ( interpret
    ) where

import Ast
import Symtab
import Control.Monad

run :: SymTab -> AstNode -> IO (SymTab)
run s n = return (mempty)

interpret :: Ast -> IO (SymTab)
interpret (Ast nodes) = foldM run mempty nodes
