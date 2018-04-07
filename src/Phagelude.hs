module Phagelude
    ( phagelude
    ) where

import Val
import Data.Map

numFunc :: (String, (Integer -> Integer -> Integer)) -> (String, PhageVal)
numFunc (s, f) = (s, PFunc 2 [] mempty app)
    where
        app ((PNum a) : (PNum b) : lst) tab = return $ (PNum $ f a b, tab)
        app (a : b : xs) _ = error $ "Wrong parameter types to function '"
            ++ s ++ "': " ++ "'" ++ show a ++ "', '" ++ show b ++ "'"

mathFuncs :: [(String, PhageVal)]
mathFuncs = 
    fmap numFunc
        [ ("+", (+))
        , ("-", (-))
        , ("*", (*))
        , ("/", div)
        , ("%", mod)
        , ("min", min)
        , ("max", max)
        ]

allFuncs :: [(String, PhageVal)]
allFuncs = mathFuncs

phagelude :: Map String PhageVal
phagelude = Prelude.foldl (\m (k, v) -> insert k v m) mempty allFuncs
