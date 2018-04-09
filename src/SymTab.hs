module SymTab
    ( SymTab(..)
    ) where

import Data.Map

type SymTab a = Map String a
