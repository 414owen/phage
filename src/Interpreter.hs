module Interpreter
    ( interpret
    , eval
    , block
    , tr
    ) where

import Err
import Val
import SymTab
import Debug.Trace
import Data.Monoid
import Control.Monad
import Control.Monad.Trans.Except
import Data.Map

tr :: Show a => String -> a -> a
tr s a = trace (s <> ": " <> show a) a

lkp :: SymTab PhageVal -> String -> Either PhageErr PhageVal
lkp tab str = case Data.Map.lookup str tab of
    Just v -> Right v
    _      -> Left ("Variable '" <> str <> "' not defined")

reduceFunc :: PhageVal -> [PhageVal] -> PhageVal
reduceFunc (PFunc arity params env fn) [] = PFunc arity params env fn
reduceFunc (PFunc arity params env fn) (v : xs)
    = reduceFunc (PFunc (arity - 1) (v : params) env fn) xs

type Acc = ExceptT PhageErr IO ([PhageVal], SymTab PhageVal)

-- in a block, symtab updates carry through the scope they are defined in
block :: SymTab PhageVal -> [PhageVal] -> ExceptT PhageErr IO [PhageVal]
block tab blk = reverse . fst <$> foldM folder ([], tab) blk
    where
        folder :: ([PhageVal], SymTab PhageVal) -> PhageVal -> Acc
        folder (acc, tab) node = eval tab node >>= \(v, t) -> pure (v:acc, t)

-- old version of block
{- block tab [] = pure [] -}
{- block tab (n:ns) = eval tab n >>= \(v, t) -> (v:) <$> block t ns -}

eval :: SymTab PhageVal -> PhageVal -> ExceptT PhageErr IO (PhageVal, SymTab PhageVal)
eval tab (PAtom str) = (,tab) <$> ExceptT (pure (lkp tab str))
eval tab (PList (fname : params)) = eval tab fname
    >>= \(fn, ftab) -> case fn of
        (PFunc a p t f) -> block tab params
            >>= \ps -> case reduceFunc fn ps of
                PFunc a p env f | a <= 0 -> (,tab) <$> f (reverse p) env
                a -> return (a, tab)
        (PForm a f) | a <= length params -> f params tab
        (PForm a f) -> ExceptT $ return $ Left "Not enough params to form"
        a -> ExceptT $ return $ Left
             ("Tried to call a non-function: " <> show a)
eval tab thing = return (thing, tab)

interpret ::
    SymTab PhageVal
    -> [PhageVal]
    -> ExceptT PhageErr IO (PhageVal, SymTab PhageVal)
interpret prelude nodes = foldM (eval . snd) (PList [], prelude) nodes
