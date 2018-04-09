module Phagelude
    ( phagelude
    ) where

import Err
import Val
import SymTab
import Data.Map
import Data.Maybe
import Control.Monad.Trans.Except

typeMess :: Integer -> String -> PhageVal -> String
typeMess n exp val = "Parameter " ++ show n ++ " has the wrong type,\
    \ expecting '" ++ exp ++ "' but got '" ++ typeName val ++ "'"

-- create a binary function over phage values
createBinFunc ::
    ((PhageVal -> Maybe a), String)
    -> ((PhageVal -> Maybe b), String)
    -> (c -> PhageVal)
    -> (a -> b -> c)
    -> (PhageVal)
createBinFunc (fromA, aStr) (fromB, bStr) toC fn = PFunc 2 [] mempty nf
    where
        nf :: [PhageVal] -> SymTab PhageVal -> ExceptT PhageErr IO (PhageVal, SymTab PhageVal)
        nf [a, b] tab = case (fromA a, fromB b) of
            (Nothing, _) -> error $ typeMess 1 aStr a
            (_, Nothing) -> error $ typeMess 2 bStr b
            (Just a, Just b) -> return (toC $ fn a b, tab)
        nf _ _  = error "Too many parameters applied to binary function"

fromNum :: (PhageVal -> Maybe Integer)
fromNum (PNum a) = Just a
fromNum _ = Nothing

fromNumTup = (fromNum, "Number")

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd fn (a, b) = (a, fn b)

createOnTwoInts = createBinFunc fromNumTup fromNumTup

arith :: [(String, PhageVal)]
arith =
    fmap (mapSnd (createOnTwoInts PNum))
        [ ("+", (+))
        , ("-", (-))
        , ("*", (*))
        , ("/", div)
        , ("%", mod)
        , ("min", min)
        , ("max", max)
        ]

comparison :: [(String, PhageVal)]
comparison =
    fmap (mapSnd (createOnTwoInts PBool))
        [ ("=", (==))
        , ("<", (<))
        , (">", (>))
        , ("<=", (<=))
        , (">=", (>=))
        ]

consts :: [(String, PhageVal)]
consts =
    [ ("zero", PNum 0)
    , ("true", PBool True)
    , ("false", PBool False)
    ]

allVals :: [(String, PhageVal)]
allVals = concat
    [ arith
    , comparison
    , consts
    , [("print", PFunc 1 [] mempty prnt)]
    ]
        where
        prnt :: [PhageVal]
            -> SymTab PhageVal
            -> ExceptT PhageErr IO (PhageVal, SymTab PhageVal)
        prnt []  t = ret (putStrLn "") (PNil, t)
        prnt lst t = ret (mapM print lst) (last lst, t)

        ret :: IO a -> b -> ExceptT PhageErr IO b
        ret a b = ExceptT $ fmap (const (Right b)) $ a


phagelude :: Map String PhageVal
phagelude = Prelude.foldl (\m (k, v) -> insert k v m) mempty allVals
