module Symtab
    ( new
    , SymTab
    , update
    ) where

-- This exists in case we ever decide to replace Data.Map

import Data.Map
import Ast

type SymTab = Map String AstNode


