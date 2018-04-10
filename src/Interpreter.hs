module Interpreter
    ( interpret
    , eval
    ) where

import Err
import Ast
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
block :: SymTab PhageVal -> [AstNode] -> ExceptT PhageErr IO [PhageVal]
block tab [] = ExceptT (return $ Right [])
block tab (n:[]) = fmap (pure . fst) $ eval tab n
block tab (n:ns) = eval tab n >>= \(v, t) -> fmap (v:) $ block t ns

eval :: SymTab PhageVal -> AstNode -> ExceptT PhageErr IO (PhageVal, SymTab PhageVal)
eval tab (ANum a) = return (PNum a, tab)
eval tab (AAtom str) = fmap (,tab) $ ExceptT (return (lkp tab str))
eval tab (AList [])  = return (PList [], tab)
eval tab (AList (fn : params)) = eval tab fn
    >>= \(fn, ftab) -> case fn of
        (PFunc a p t f) -> block tab params
            >>= \(ps) -> case reduceFunc fn ps of
                PFunc a p e f | a <= 0 -> f (reverse p) e
                a -> return (a, tab)
        (PForm a f) | a <= length params -> f params tab
        (PForm a f) -> ExceptT $ return $ Left "Not enough params to form"

interpret :: SymTab PhageVal -> Ast -> ExceptT PhageErr IO PhageVal
interpret prelude (Ast nodes) = fmap fst $ foldM (eval . snd) (PList [], prelude) nodes
