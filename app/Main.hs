module Main where

import Parser
import Text.Megaparsec
import System.Environment

source :: [String] -> IO (String, String)
source (_:file:_) = (file,) <$> readFile file
source _ = ("stdin",) <$> getContents

main :: IO ()
main = getArgs >>=
    source >>=
    \(s, i) -> parseTest parseAst i
