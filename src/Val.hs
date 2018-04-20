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
import Data.Monoid
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
    | PStr String
    | PFunc Int [PhageVal] (SymTab PhageVal) PhageFunc
    | PForm Int PhageForm

spacedShow :: String -> [PhageVal] -> String
spacedShow space els = intercalate space (show <$> els)

instance Eq PhageVal where
    (==) (PNum  a) (PNum  b) = a == b
    (==) (PChar a) (PChar b) = a == a
    (==) (PAtom a) (PAtom b) = a == b
    (==) (PList a) (PList b) = a == b
    (==) (PBool a) (PBool b) = a == b
    (==) (PStr  a) (PStr  b) = a == b
    (==) _         _         = False

instance Show PhageVal where
    show (PNum a)         = show a
    show (PChar c)        = pure c
    show (PAtom a)        = a
    show (PStr s)         = s
    show (PBool True)     = "true"
    show (PBool False)    = "false"
    show (PList els)      = "(" <> spacedShow " " els <> ")"
    show (PForm _ _)      = "<form>"
    show (PFunc a p s fn) = "<func | arity: " <> show a <>
        ", bound params: [" <> spacedShow ", " p <> "]>"

typeName :: PhageVal -> String
typeName (PNum _)        = "num"
typeName (PAtom _)       = "atom"
typeName (PList [])      = "nil"
typeName (PList _)       = "list"
typeName (PBool _)       = "bool"
typeName PFunc{}         = "func"
typeName (PForm _ _)     = "form"
