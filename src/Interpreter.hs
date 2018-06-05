module Interpreter
    ( interpret
    , eval
    , block
    , lastBlock
    , tr
    , apply
    , BlockRes
    ) where

import Err
import Val
import Safe
import Debug.Trace
import Data.Monoid
import Data.Maybe
import Data.Tuple.Lazy
import Control.Monad
import Control.Monad.Trans.Except
import Data.Map hiding (null)
import Prelude hiding (lookup)

tr :: Show a => String -> a -> a
tr s a = trace (s <> ": " <> show a) a

lkp :: SymTab -> String -> ExceptT PhageErr IO PhageVal
lkp tab str = case lookup str tab of
        Just v -> v
        _      -> throwE $ "Variable " <> str <> " not in scope"

reduceFunc :: PhageVal -> [PhageVal] -> PhageVal
reduceFunc f [] = f
reduceFunc f@(PForm{arity=a, bound=par}) (v : xs)
    = reduceFunc (f {arity = a - 1, bound = v : par}) xs

apply :: SymTab -> PhageVal -> [PhageVal] -> PhageRes
apply tab form@(PForm{paramap=pmap, name=n}) ps =
    case reduceFunc form ps of
        nform@PForm{arity=a, bound=p, form=f}
            | a <= 0 -> f tab (reverse p)
        a -> return ([], a)
apply tab a ps = throwE ("Tried to call a non-function: " <> show a)

realeval :: SymTab -> PhageVal -> PhageRes
realeval tab (PAtom str) = ([],) <$> lkp tab str
realeval tab (PList (fname : params)) = eval tab fname
    >>= \(e1, fn) -> case fn of
        f@PForm{paramap=pmap} -> foldM
            (\(e2, l) p -> pmap (newTabM e2 tab) p >>=
                \(e3, v) -> pure (e2 <> e3, v : l))
            (e1, []) params
        _ -> pure ([], params)
    >>= \(e4, vs) -> apply tab fn (reverse vs)
    >>= \(e5, v) -> pure (e4 <> e5, v)
realeval t (PQList l) = pure ([], PList l)
realeval t thing = pure ([], thing)

eval :: SymTab -> PhageVal -> PhageRes
eval tab val = ExceptT $ runExceptT (realeval tab val)
    >>= \res -> case res of
        Left err -> fmap (const $ Left ("in " <> show val)) $ putStrLn err
        Right res -> pure $ Right res

type BlockRes = ExceptT PhageErr IO ([SymEdit], [PhageVal])

-- in a block, symtab updates carry through the scope they are defined in
block :: SymTab -> [PhageVal] -> BlockRes
block _   []    = throwE "Tried to evaluate block with no nodes"
block tab nodes = blockrec [] tab nodes
    where
    blockrec :: [SymEdit] -> SymTab -> [PhageVal] -> BlockRes
    blockrec eds tab (n:ns) = let nt = newTabM eds tab in
        eval nt n >>= \(neds, res) ->
            (\(edacc, resacc) -> (neds <> edacc, res : resacc))
                <$> if null ns then pure ([], []) else blockrec neds nt ns

lastBlock :: SymTab -> [PhageVal] -> PhageRes
lastBlock tab nodes = mapSnd (lastDef $ PList []) <$> block tab nodes

interpret :: SymTab -> [PhageVal] -> BlockRes
interpret = block
