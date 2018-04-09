module Interpreter
    ( interpret
    ) where

import Err
import Ast
import Val
import Phagelude
import SymTab
import Control.Monad
import Control.Monad.Trans.Except
import Data.Map

lkp :: SymTab PhageVal -> String -> Either PhageErr PhageVal
lkp tab str = case Data.Map.lookup str tab of
    Just v -> return v
    _      -> Left ("Variable '" ++ str ++ "' not defined")

reduceFunc :: [PhageVal] -> PhageVal
reduceFunc (PFunc 0 params env fn : [])
    = PFunc 0 params env fn
reduceFunc (PFunc arity params env fn : [])
    = PFunc arity params env fn
reduceFunc (PFunc arity params env fn : v : xs)
    = reduceFunc $ (PFunc (arity - 1) (v : params) env fn : xs)

-- in a block, symtab updates carry through the scope they are defined in
block :: SymTab PhageVal -> [AstNode] -> ExceptT PhageErr IO [PhageVal]
block tab (n:[]) = fmap (pure . fst) $ eval tab n
block tab (n:ns) = eval tab n >>= \(v, t) -> fmap (v:) $ block t ns

eval :: SymTab PhageVal -> AstNode -> ExceptT PhageErr IO (PhageVal, SymTab PhageVal)
eval tab (ANum a) = return (PNum a, tab)
eval tab (AAtom str) = ExceptT (return (lkp tab str)) >>= return . (,tab)
eval tab (AList [])  = return (PNil, tab)
eval tab (AList lst) = block tab lst
    >>= \lst -> case reduceFunc lst of
        PFunc a p e f | a <= 0 -> f (reverse p) e
        a -> return (a, tab)

interpret :: Ast -> ExceptT PhageErr IO PhageVal
interpret (Ast nodes) = fmap fst $ foldM (eval . snd) (PNil, phagelude) nodes
