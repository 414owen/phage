module Val
    ( PhageVal(..)
    , FormVal (..)
    , arity
    , bound
    , paramap
    , form
    , name
    , func
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
import Data.Map hiding (foldl)
import Data.Tuple.Lazy
import Data.List hiding (insert)
import Data.Maybe
import Data.Monoid
import Control.Monad.Trans.Except
import Lens.Micro (Lens', (^.))

type SymEdit = (String, ExceptT PhageErr IO PhageVal)
type PRes = ([SymEdit], PhageVal)
type PhageRes = ExceptT PhageErr IO PRes

type PhageForm =
        -- caller environment
           SymTab
        -- returns a result, and a new caller environment
        -> [PhageVal]
        -> PhageRes

data FormVal = FormVal
  { fvArity   :: Int
  , fvBound   :: [PhageVal]
  , fvParamap :: SymTab -> PhageVal -> PhageRes
  , fvForm    :: PhageForm
  , fvName    :: Maybe String
  , fvFunc    :: Bool
  }

arity :: Lens' FormVal Int
arity lens fv@(FormVal {fvArity = val}) =
  fmap (\newVal -> fv { fvArity = newVal}) (lens val)

bound :: Lens' FormVal [PhageVal]
bound lens fv@(FormVal {fvBound = val}) =
  fmap (\newVal -> fv {fvBound = newVal}) (lens val)

paramap :: Lens' FormVal (SymTab -> PhageVal -> PhageRes)
paramap lens fv@(FormVal {fvParamap = val}) =
  fmap (\newVal -> fv {fvParamap = newVal}) (lens val)

form :: Lens' FormVal PhageForm
form lens fv@(FormVal {fvForm = val}) =
  fmap (\newVal -> fv {fvForm = newVal}) (lens val)

name :: Lens' FormVal (Maybe String)
name lens fv@(FormVal {fvName = val}) =
  fmap (\newVal -> fv {fvName = newVal}) (lens val)

func :: Lens' FormVal Bool
func lens fv@(FormVal {fvFunc = val}) =
  fmap (\newVal -> fv {fvFunc = newVal}) (lens val)

data PhageVal
    = PNum Integer
    | PChar Char
    | PAtom String
    | PList [PhageVal]
    | PQList [PhageVal]
    | PBool Bool
    -- arity, bound params, bound env, form
    | PForm FormVal

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
    (==) (PChar a) (PChar b) = a == b
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
    show (PForm fv) =
        "<" <> (if fv ^. func then "func" else "form") <>
        (fromMaybe "" (fmap (" "<>) $ fv ^. name)) <> " | arity: " <>
        (show $ fv ^. arity) <> ", bound params: " <> (show . length $ fv ^. bound) <> ">"

typeName :: PhageVal -> String
typeName (PNum _)        = "num"
typeName (PAtom _)       = "atom"
typeName (PList [])      = "nil"
typeName (PList _)       = "list"
typeName (PBool _)       = "bool"
typeName (PChar _)       = "name"
typeName (PQList _)      = "qlist"
typeName (PForm _)       = "form"
