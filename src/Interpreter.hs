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
import Lens.Micro ((^.), (%~), (&))
import Data.Map hiding (null)
import Prelude hiding (lookup)

tr :: Show a => String -> a -> a
tr s a = trace (s <> ": " <> show a) a

lkp :: SymTab -> String -> ExceptT PhageErr IO PhageVal
lkp tab str = case lookup str tab of
        Just v -> v
        _      -> throwE $ "Variable " <> str <> " not in scope"

reduceFunc :: PhageVal -> [PhageVal] -> PhageVal
reduceFunc (PForm fv) (v : xs) =
  reduceFunc (PForm $ fv & arity %~ (`subtract` 1) & bound %~ (v:)) xs
reduceFunc f _ = f

apply :: SymTab -> PhageVal -> [PhageVal] -> PhageRes
apply tab pform@(PForm _) ps =
    case reduceFunc pform ps of
        PForm fv'
            | (fv' ^. arity) <= 0 -> (fv' ^. form) tab (reverse $ fv' ^. bound)
        a -> return ([], a)
apply _ a _ = throwE ("Tried to call a non-function: " <> show a)

realeval :: SymTab -> PhageVal -> PhageRes
realeval tab (PAtom str) = ([],) <$> lkp tab str
realeval tab (PList (fname : params)) = eval tab fname
    >>= \(e1, fn) -> case fn of
        PForm fv -> foldM
            (\(e2, l) p -> (fv ^. paramap) (newTabM e2 tab) p >>=
                \(e3, v) -> pure (e2 <> e3, v : l))
            (e1, []) params
        _ -> pure ([], params)
    >>= \(e4, vs) -> apply tab fn (reverse vs)
    >>= \(e5, v) -> pure (e4 <> e5, v)
realeval _ (PQList l) = pure ([], PList l)
realeval _ thing = pure ([], thing)

eval :: SymTab -> PhageVal -> PhageRes
eval tab val = ExceptT $ runExceptT (realeval tab val)
    >>= \res -> case res of
        Left err -> fmap (const $ Left ("in " <> show val)) $ putStrLn err
        Right r -> pure $ Right r

type BlockRes = ExceptT PhageErr IO ([SymEdit], [PhageVal])

-- in a block, symtab updates carry through the scope they are defined in
block :: SymTab -> [PhageVal] -> BlockRes
block tab nodes = blockrec [] tab nodes
    where
    blockrec :: [SymEdit] -> SymTab -> [PhageVal] -> BlockRes
    blockrec eds _ (n:ns) = let nt = newTabM eds tab in
        eval nt n >>= \(neds, res) ->
            (\(edacc, resacc) -> (neds <> edacc, res : resacc))
                <$> if null ns then pure ([], []) else blockrec neds nt ns
    blockrec _ _ [] = throwE "Tried to evaluate block with no nodes"

lastBlock :: SymTab -> [PhageVal] -> PhageRes
lastBlock tab nodes = mapSnd (lastDef $ PList []) <$> block tab nodes

interpret :: SymTab -> [PhageVal] -> BlockRes
interpret = block
