module Val
    ( PhageVal(..)
    , SymTab
    ) where

import Text.Show.Functions
import Ast
import Data.Map
import Data.List

data PhageVal
    = PNil
    | PNum Integer
    | PAtom String
    | PList [PhageVal]
    | PBool Bool
    | PFunc Int [PhageVal]
        SymTab
        ([PhageVal]
        -> SymTab
        -> IO (PhageVal, SymTab))

spacedShow :: String -> [PhageVal] -> String
spacedShow space els = concat $ intersperse space (fmap show els)

instance Show PhageVal where
    show (PNil) = "()"
    show (PNum a) = show a
    show (PAtom a) = a
    show (PBool True) = "true"
    show (PList els) = "(" ++ spacedShow " " els ++ ")"
    show (PFunc a p s fn) = "<func | arity: " ++ show a ++ ", bound params: [" ++
        spacedShow ", " p ++ "]>"

type SymTab = Map String PhageVal
