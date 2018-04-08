module Interpreter
    ( interpret
    ) where

import Ast
import Phagelude
import Val
import Control.Monad
import Data.Map

lkp :: SymTab -> String -> IO (PhageVal)
lkp tab str = case Data.Map.lookup str tab of
    Just v -> return v
    _      -> fail ("Variable '" ++ str ++ "' not defined")

reduceFunc :: [PhageVal] -> PhageVal
reduceFunc (PFunc 0 params env fn : [])
    = PFunc 0 params env fn
reduceFunc (PFunc arity params env fn : [])
    = PFunc arity params env fn
reduceFunc (PFunc arity params env fn : v : xs)
    = reduceFunc $ (PFunc (arity - 1) (v : params) env fn : xs)

-- in a block, symtab updates carry through the scope they are defined in
block :: SymTab -> [AstNode] -> IO [PhageVal]
block tab (n:[]) = fmap (pure . fst) $ eval tab n
block tab (n:ns) = eval tab n >>= \(v, t) -> fmap (v:) $ block t ns

eval :: SymTab -> AstNode -> IO (PhageVal, SymTab)
eval tab (ANum a) = return (PNum a, tab)
eval tab (AAtom str) = lkp tab str >>= return . (,tab)
eval tab (AList [])  = return (PNil, tab)
eval tab (AList lst) = block tab lst
    >>= \lst -> case reduceFunc lst of
        PFunc 0 p e f -> f (reverse p) e
        a -> return (a, tab)

interpret :: Ast -> IO (PhageVal)
interpret (Ast nodes) = fmap fst $ foldM (eval . snd) (PNil, phagelude) nodes
