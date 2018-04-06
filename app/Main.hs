module Main where

import Parser
import Interpreter
import Text.Megaparsec
import System.Environment

source :: [String] -> IO (String, String)
source (_:file:_) = (file,) <$> readFile file
source _ = ("stdin",) <$> getContents

main :: IO ()
main = getArgs >>=
    source >>=
    \(s, i) -> case parse parseAst s i of
        Left  err ->
               putStrLn "Parse error:"
            >> print err
        Right ast ->
               print ast
            >> interpret ast
            >>= print
            >> return ()
