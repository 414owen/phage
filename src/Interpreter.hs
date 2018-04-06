module Interpreter
    ( interpret
    ) where

import Ast
import Symtab
import Val
import Control.Monad
import Data.Map

lkp :: SymTab -> String -> IO (PhageVal)
lkp tab str = case Data.Map.lookup str tab of
    Just v -> return v
    _      -> fail ("Variable " ++ str ++ " not defined")

reduceFunc :: [(PhageVal, SymTab)] -> (PhageVal, SymTab)
reduceFunc ((fn, tab):lst) = (,tab) $ Prelude.foldl
    (\(PFunc arity params fn) (val, tab) ->
        PFunc (arity - 1) (val : params) fn) fn lst

eval :: SymTab -> AstNode -> IO (PhageVal, SymTab)
eval tab (ANum a) = return (PNum a, tab)
eval tab (AAtom str) = lkp tab str >>= return . (,tab)
eval tab (AList [])  = return (PNil, tab)
eval tab (AList lst) = ((forM lst (eval tab)) :: IO [(PhageVal, SymTab)])
    >>= \lst -> return $ reduceFunc lst

interpret :: Ast -> IO (PhageVal)
interpret (Ast nodes) = fmap fst $ foldM (eval . snd) (PNil, base) nodes
