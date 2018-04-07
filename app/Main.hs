module Main where

import Parser
import Interpreter
import Data.List
import Data.Char
import Data.Maybe
import Text.Megaparsec
import System.Environment
import System.IO

source :: [String] -> IO (String, String)
source (_:file:_) = (file,) <$> readFile file
source _ = ("stdin",) <$> getContents

display :: String -> String -> IO ()
display s i = case parse parseAst s i of
    Left  err ->
           putStrLn "Parse error:"
        >> print err
    Right ast ->
           print ast
        >> interpret ast
        >>= print

repl :: Bool -> IO ()
repl unicode =
        putStr ((if unicode then '\x03BB' else '>') : " ")
    >>  hFlush stdout
    >>  getLine
    >>= display "stdin"
    >>  repl unicode

main :: IO ()
main =
    mapM lookupEnv ["LANG", "LC_ALL", "LC_CTYPE"]
    >>= return . any (isInfixOf "UTF" . map toUpper) . catMaybes
    >>= \unicode -> getArgs
    >>= \args ->
        case args of
            (a : b : xs) -> readFile b >>= display b
            _ -> repl unicode
