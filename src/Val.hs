module Val
    ( PhageVal(..)
    ) where

import Text.Show.Functions
import Ast

data PhageVal =
    -- empty list from ast
      PNil
    | PNum Integer
    | PAtom String
    | PList [PhageVal]
    | PBool Bool

    -- functions can update the symbol table by one element
    | PFunc Int [PhageVal] ([PhageVal] -> (PhageVal, Maybe (String, PhageVal)))
    deriving (Show)
