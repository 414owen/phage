module Interpreter
    ( interpret
    , eval
    , block
    , tr
    , apply
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
block tab [] = pure []
block tab (n:ns) = eval tab n >>= \(v, t) -> (v:) <$> block t ns

apply ::
    SymTab PhageVal
    -> PhageVal   -- fn / form
    -> [PhageVal] -- params
    -> ExceptT PhageErr IO (PhageVal, SymTab PhageVal)
apply tab (PFunc a p t f) ps =
    case reduceFunc (PFunc a p t f) ps of
        PFunc a p env f | a <= 0 -> (,tab) <$> f (reverse p) env
        a -> return (a, tab)
apply tab (PForm a f) ps | a <= length ps = f ps tab
apply tab (PForm a f) ps = ExceptT $ return $ Left "Not enough params to form"
apply tab a ps = ExceptT $ return $ Left
     ("Tried to call a non-function: " <> show a)

realeval ::
       SymTab PhageVal
    -> PhageVal
    -> ExceptT PhageErr IO (PhageVal, SymTab PhageVal)
realeval tab (PAtom str) = (,tab) <$> ExceptT (pure (lkp tab str))
realeval tab (PList (fname : params)) = eval tab fname
    >>= \(fn, ftab) -> case fn of
        (PFunc a b c d) -> (block tab params) >>= apply tab (PFunc a b c d)
        a -> apply tab a params
realeval tab thing = return (thing, tab)

eval :: SymTab PhageVal
    -> PhageVal
    -> ExceptT PhageErr IO (PhageVal, SymTab PhageVal)
eval tab val = ExceptT $ runExceptT (realeval tab val)
    >>= \res -> case res of
        Left err -> fmap (const $ Left ("in " <> show val)) $ putStrLn err
        Right res -> pure $ Right res

interpret ::
    SymTab PhageVal
    -> [PhageVal]
    -> ExceptT PhageErr IO (PhageVal, SymTab PhageVal)
interpret prelude nodes = foldM (eval . snd) (PList [], prelude) nodes
