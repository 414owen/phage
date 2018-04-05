module Symtab
    ( empty
    , base
    , SymTab
    , update
    ) where

-- This exists in case we ever decide to replace Data.Map

import Data.Map
import Val

type SymTab = Map String PhageVal

new :: SymTab
new = mempty

-- TODO put the prelude in base

base :: SymTab
base = Prelude.foldl (\m (k, v) -> insert k v m) new
    [
    ]
