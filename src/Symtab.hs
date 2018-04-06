module Symtab
    ( empty
    , base
    , SymTab
    , update
    ) where

-- This exists in case we ever decide to replace Data.Map

import Val
import Data.Map
import Data.Maybe

type SymTab = Map String PhageVal

new :: SymTab
new = mempty

-- TODO put the prelude in base
-- reminder that bools are added here

mkFunc :: (String, Int, ([PhageVal] -> PhageVal)) -> (String, PhageVal)
mkFunc (s, a, f) = (s, PFunc a [] ((,Nothing) . f))

basicFuncs = 
    fmap mkFunc [ ("+", 2, (\[PNum a, PNum b] -> (PNum (a + b))))
    ]

base :: SymTab
base = Prelude.foldl (\m (k, v) -> insert k v m) new basicFuncs
