module Val
    ( PhageVal
    ) where

import Text.Show.Functions
import Ast

data PhageVal =
    -- empty list from ast
      Nil
    | Number Integer
    | Atom String
    | List [PhageVal]
    | Bool Bool
    -- functions can update the symbol table by one element
    | Function ([PhageVal] -> (PhageVal, Maybe (String, PhageVal)))
    deriving (Show)
