module Val
    ( PhageVal(..)
    , typeName
    ) where

import Err
import SymTab
import Text.Show.Functions
import Ast
import Data.Map
import Data.List
import Control.Monad.Trans.Except

data PhageVal
    = PNil
    | PNum Integer
    | PAtom String
    | PList [PhageVal]
    | PBool Bool
    | PFunc Int [PhageVal]
        (SymTab PhageVal)
        ([PhageVal]
        -> (SymTab PhageVal)
        -> ExceptT PhageErr IO (PhageVal, SymTab PhageVal))

spacedShow :: String -> [PhageVal] -> String
spacedShow space els = concat $ intersperse space (fmap show els)

instance Show PhageVal where
    show (PNil) = "()"
    show (PNum a) = show a
    show (PAtom a) = a
    show (PBool True) = "true"
    show (PBool False) = "false"
    show (PList els) = "(" ++ spacedShow " " els ++ ")"
    show (PFunc a p s fn) = "<func | arity: " ++ show a ++
        ", bound params: [" ++ spacedShow ", " p ++ "]>"

typeName :: PhageVal -> String
typeName PNil = "nil"
typeName (PNum _) = "num"
typeName (PAtom _) = "atom"
typeName (PList _) = "list"
typeName (PBool _) = "bool"
typeName (PFunc _ _ _ _) = "func"
