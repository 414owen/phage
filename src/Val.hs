module Val
    ( PhageVal(..)
    , SymTab
    , SymEdit
    , PRes
    , PhageRes
    , newTab
    , newTabM
    , typeName
    , PhageForm
    ) where

import Err
import Text.Show.Functions
import Data.Map hiding (foldl)
import Data.Tuple.Lazy
import Data.List hiding (insert)
import Data.Maybe
import Data.Monoid
import Control.Monad.Trans.Except

type SymEdit = (String, ExceptT PhageErr IO PhageVal)
type PRes = ([SymEdit], PhageVal)
type PhageRes = ExceptT PhageErr IO PRes

type PhageForm =
        -- caller environment
           SymTab
        -- returns a result, and a new caller environment
        -> [PhageVal]
        -> PhageRes

data PhageVal
    = PNum Integer
    | PChar Char
    | PAtom String
    | PList [PhageVal]
    | PQList [PhageVal]
    | PBool Bool
    -- arity, bound params, bound env, form
    | PForm
      { arity   :: Int
      , bound   :: [PhageVal]
      , paramap :: SymTab -> PhageVal -> PhageRes
      , form    :: PhageForm
      , name    :: Maybe String
      , func    :: Bool
      }

-- this scope's definitions, to be carried
type SymTab = Map String (ExceptT PhageErr IO PhageVal)

newTabM :: [SymEdit] -> SymTab -> SymTab
newTabM vs t = foldl (\acc (s, v) -> insert s v acc) t vs

newTab :: [(String, PhageVal)] -> SymTab -> SymTab
newTab vs t = newTabM (fmap (mapSnd pure) vs) t

spacedShow :: String -> [PhageVal] -> String
spacedShow space els = intercalate space (show <$> els)

instance Eq PhageVal where
    (==) (PNum  a) (PNum  b) = a == b
    (==) (PChar a) (PChar b) = a == a
    (==) (PAtom a) (PAtom b) = a == b
    (==) (PList a) (PList b) = a == b
    (==) (PBool a) (PBool b) = a == b
    (==) _         _         = False

instance Show PhageVal where
    show (PNum a)         = show a
    show (PChar c)        = pure c
    show (PAtom a)        = a
    show (PBool True)     = "true"
    show (PBool False)    = "false"
    show (PList els)      = "(" <> spacedShow " " els <> ")"
    show (PQList els)      = "{" <> spacedShow " " els <> "}"
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
