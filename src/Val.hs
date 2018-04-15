module Val
    ( PhageVal(..)
    , typeName
    , PhageFunc
    , PhageForm
    ) where

import Err
import SymTab
import Text.Show.Functions
import Data.Map
import Data.List
import Control.Monad.Trans.Except

type PhageFunc =
            [PhageVal]
        ->  SymTab PhageVal
        ->  ExceptT PhageErr IO PhageVal

type PhageForm =
            [PhageVal]
        ->  SymTab PhageVal
        ->  ExceptT PhageErr IO (PhageVal, SymTab PhageVal)

data PhageVal
    = PNum Integer
    | PChar Char
    | PAtom String
    | PList [PhageVal]
    | PBool Bool
    | PFunc Int [PhageVal] (SymTab PhageVal) PhageFunc
    | PForm Int PhageForm

spacedShow :: String -> [PhageVal] -> String
spacedShow space els = intercalate space (show <$> els)

instance Show PhageVal where
    show (PChar c) = show c
    show (PNum a) = show a
    show (PAtom a) = a
    show (PBool True) = "true"
    show (PBool False) = "false"
    show (PList els) = "(" ++ spacedShow " " els ++ ")"
    show (PForm _ _) = "<form>"
    show (PFunc a p s fn) = "<func | arity: " ++ show a ++
        ", bound params: [" ++ spacedShow ", " p ++ "]>"

typeName :: PhageVal -> String
typeName (PNum _)        = "num"
typeName (PAtom _)       = "atom"
typeName (PList [])      = "nil"
typeName (PList _)       = "list"
typeName (PBool _)       = "bool"
typeName PFunc{}         = "func"
typeName (PForm _ _)     = "form"
