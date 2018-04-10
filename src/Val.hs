module Val
    ( PhageVal(..)
    , typeName
    , PhageFunc
    , PhageForm
    ) where

import Err
import SymTab
import Text.Show.Functions
import Ast
import Data.Map
import Data.List
import Control.Monad.Trans.Except

type PhageFunc =
            [PhageVal]
        ->  (SymTab PhageVal)
        ->  ExceptT PhageErr IO (PhageVal, SymTab PhageVal)

type PhageForm =
            [AstNode]
        ->  (SymTab PhageVal)
        ->  ExceptT PhageErr IO (PhageVal, SymTab PhageVal)

data PhageVal
    = PNum Integer
    | PAtom String
    | PList [PhageVal]
    | PBool Bool
    | PFunc Int [PhageVal] (SymTab PhageVal) PhageFunc
    | PForm Int (SymTab PhageVal) PhageForm

spacedShow :: String -> [PhageVal] -> String
spacedShow space els = concat $ intersperse space (fmap show els)

instance Show PhageVal where
    show (PNum a) = show a
    show (PAtom a) = a
    show (PBool True) = "true"
    show (PBool False) = "false"
    show (PList els) = "(" ++ spacedShow " " els ++ ")"
    show (PFunc a p s fn) = "<func | arity: " ++ show a ++
        ", bound params: [" ++ spacedShow ", " p ++ "]>"

typeName :: PhageVal -> String
typeName (PNum _)        = "num"
typeName (PAtom _)       = "atom"
typeName (PList [])      = "nil"
typeName (PList _)       = "list"
typeName (PBool _)       = "bool"
typeName (PFunc _ _ _ _) = "func"
