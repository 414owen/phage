module Interpreter
    ( interpret
    , eval
    , block
    , tr
    , apply
    ) where

import Err
import Val
import Debug.Trace
import Data.Monoid
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Except
import Data.Map

tr :: Show a => String -> a -> a
tr s a = trace (s <> ": " <> show a) a

lkp :: SymTab -> String -> Either PhageErr PhageVal
lkp tab str = case Data.Map.lookup str tab of
    Just v -> Right v
    _      -> Left ("Variable '" <> str <> "' not defined")

reduceFunc :: PhageVal -> [PhageVal] -> PhageVal
reduceFunc f [] = f
reduceFunc f@(PForm{arity=a, bound=par}) (v : xs)
    = reduceFunc (f {arity = a - 1, bound = v : par}) xs

type Acc = ExceptT PhageErr IO ([PhageVal], SymTab)

-- in a block, symtab updates carry through the scope they are defined in
block :: SymTab -> [PhageVal] -> ExceptT PhageErr IO [PhageVal]
block tab [] = pure []
block tab (n:ns) = eval tab n >>= \(v, t) -> (v:) <$> block t ns

apply :: SymTab -> PhageVal -> [PhageVal] -> PhageRes
apply tab form@(PForm{paramap=pmap, name=n}) ps =
    case reduceFunc form ps of
        nform@PForm{arity=a, bound=p, form=f}
            | a <= 0 -> f (reverse p) tab
        a -> return (a, tab)
apply tab a ps = ExceptT $ return $ Left
     ("Tried to call a non-function: " <> show a)

realeval :: SymTab -> PhageVal -> PhageRes
realeval tab (PAtom str) = (,tab) <$> ExceptT (pure (lkp tab str))
realeval tab (PList (fname : params)) = eval tab fname
    >>= \(fn, ftab) -> case fn of
        f@PForm{paramap=pmap} -> mapM (fmap fst . pmap tab) params
        _ -> pure params
    >>= \params -> apply tab fn params
realeval tab thing = return (thing, tab)

eval :: SymTab -> PhageVal -> PhageRes
eval tab val = ExceptT $ runExceptT (realeval tab val)
    >>= \res -> case res of
        Left err -> fmap (const $ Left ("in " <> show val)) $ putStrLn err
        Right res -> pure $ Right res

interpret ::
    SymTab
    -> [PhageVal]
    -> ExceptT PhageErr IO (PhageVal, SymTab)
interpret prelude nodes = foldM (eval . snd) (PList [], prelude) nodes
