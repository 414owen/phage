module Val
    ( PhageVal(..)
    , SymTab
    ) where

import Text.Show.Functions
import Ast
import Data.Map

data PhageVal =

    -- empty list from ast
      PNil
    | PNum Integer
    | PAtom String
    | PList [PhageVal]
    | PBool Bool

    -- functions can update the symbol table by one element
    | PFunc Int [PhageVal]
        SymTab
        ([PhageVal]
        -> SymTab
        -> IO (PhageVal, SymTab))
    deriving (Show)

type SymTab = Map String PhageVal
