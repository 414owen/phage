module Interpreter
    ( interpret
    , eval
    , block
    ) where

import Err
import Val
import SymTab
import Data.Monoid
import Control.Monad
import Control.Monad.Trans.Except
import Data.Map

lkp :: SymTab PhageVal -> String -> Either PhageErr PhageVal
lkp tab str = case Data.Map.lookup str tab of
    Just v -> Right v
    _      -> Left ("Variable '" <> str <> "' not defined")

reduceFunc :: PhageVal -> [PhageVal] -> PhageVal
reduceFunc (PFunc 0 params env fn) [] = PFunc 0 params env fn
reduceFunc (PFunc arity params env fn) [] = PFunc arity params env fn
reduceFunc (PFunc arity params env fn) (v : xs)
    = reduceFunc (PFunc (arity - 1) (v : params) env fn) xs

-- in a block, symtab updates carry through the scope they are defined in
block :: SymTab PhageVal -> [PhageVal] -> ExceptT PhageErr IO [PhageVal]
block tab [] = ExceptT (return $ Right [])
block tab [n] = pure . fst <$> eval tab n
block tab (n:ns) = eval tab n >>= \(v, t) -> (v:) <$> block t ns

eval :: SymTab PhageVal -> PhageVal -> ExceptT PhageErr IO (PhageVal, SymTab PhageVal)
eval tab (PNum a) = return (PNum a, tab)
eval tab (PAtom str) = (,tab) <$> ExceptT (return (lkp tab str))
eval tab (PList [])  = return (PList [], tab)
eval tab (PList (fn : params)) = eval tab fn
    >>= \(fn, ftab) -> case fn of
        (PFunc a p t f) -> block tab params
            >>= \ps -> case reduceFunc fn ps of
                PFunc a p env f | a <= 0 -> (,tab) <$> f (reverse p) env
                a -> return (a, tab)
        (PForm a f) | a <= length params -> f params tab
        (PForm a f) -> ExceptT $ return $ Left "Not enough params to form"
        _ -> ExceptT $ return $ Left "Tried to call a non-function"

interpret :: SymTab PhageVal -> [PhageVal] -> ExceptT PhageErr IO (PhageVal, SymTab PhageVal)
interpret prelude nodes = foldM (eval . snd) (PList [], prelude) nodes
