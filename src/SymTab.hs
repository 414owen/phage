module SymTab
    ( SymTab(..)
    , newTab
    ) where

import Data.Map

newTab :: [(String, a)] -> SymTab a -> SymTab a
newTab vs tab = Prelude.foldl (\m (k, v) -> insert k v m) tab vs

type SymTab a = Map String a
