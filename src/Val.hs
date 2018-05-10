module Val
    ( PhageVal(..)
    , SymTab
    , PhageRes
    , newTab
    , typeName
    , PhageForm
    ) where

import Err
import Text.Show.Functions
import Data.Map
import Data.List
import Data.Maybe
import Data.Monoid
import Control.Monad.Trans.Except

type PhageRes = ExceptT PhageErr IO (PhageVal, SymTab)

type PhageForm =
           [PhageVal]
        -- caller environment
        -> SymTab
        -- returns a result, and a new caller environment
        -> PhageRes

data PhageVal
    = PNum Integer
    | PChar Char
    | PAtom String
    | PList [PhageVal]
    | PBool Bool
    | PStr String
    -- arity, bound params, bound env, form
    | PForm
      { arity   :: Int
      , bound   :: [PhageVal]
      , paramap :: SymTab -> PhageVal -> PhageRes
      , form    :: PhageForm
      , name    :: Maybe String
      , func    :: Bool
      }

type SymTab = Map String PhageVal

newTab :: [(String, PhageVal)] -> SymTab -> SymTab
newTab vs tab = Prelude.foldl (\m (k, v) -> Data.Map.insert k v m) tab vs

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
    show (PForm {func = func, arity = arity, bound = params, name = name}) =
        "<" <> (if func then "func" else "form") <>
        (fromMaybe "" (fmap (" "<>) name)) <> " | arity: " <>
        show arity <> ", bound params: " <> (show $ length params) <> ">"

typeName :: PhageVal -> String
typeName (PNum _)        = "num"
typeName (PAtom _)       = "atom"
typeName (PList [])      = "nil"
typeName (PList _)       = "list"
typeName (PBool _)       = "bool"
typeName (PForm{})     = "form"
