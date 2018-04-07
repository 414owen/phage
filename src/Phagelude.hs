module Phagelude
    ( phagelude
    ) where

import Val
import Data.Map

-- TODO put the prelude in base
-- reminder that bools are added here

mkFunc :: (String, Int, ([PhageVal] -> PhageVal)) -> (String, PhageVal)
mkFunc (s, a, f)
    = (s, PFunc a mempty mempty
    (\ps tab -> return (f ps, tab)))

basicFuncs :: [(String, PhageVal)]
basicFuncs = 
    fmap mkFunc
    [ ("+", 2, (\[PNum a, PNum b] -> (PNum (a + b))))
    ]

phagelude :: Map String PhageVal
phagelude = Prelude.foldl (\m (k, v) -> insert k v m) mempty basicFuncs
