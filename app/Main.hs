module Main where

import Ast
import Val
import Parser
import Interpreter
import SymTab
import Phagelude
import Data.List
import Data.Char
import Data.Maybe
import Text.Megaparsec
import System.Environment
import System.IO
import Data.Monoid
import Control.Monad.Trans.Except

source :: [String] -> IO (String, String)
source (_:file:_) = (file,) <$> readFile file
source _ = ("stdin",) <$> getContents

getAst :: String -> String -> Either String Ast
getAst fname src = case parse parseAst fname src of
    Left err -> Left $ "Parse error:\n" <> show err <> "\n"
    Right ast -> case ast of
        (Ast []) -> Left ""
        _ -> Right ast

run :: Bool -> SymTab PhageVal -> String -> String -> IO (SymTab PhageVal)
run repl tab fname source = case (repl, getAst fname source) of
    (_, Left e) -> const tab <$> putStr e
    (True, Right (Ast (x : y : xs))) -> const tab <$> putStrLn
        "The repl only accepts one expression at a time"
    (_, Right ast) -> runExceptT (interpret tab ast)
        >>= \res -> case res of
            Right (val, tab) ->
                const tab <$> (putStr $ if repl
                    then ">> " <> show val <> "\n"
                    else "")
            Left err ->
                const tab <$> putStrLn ("Phage Error:\n" <> err)

info = "The Phage Programming Language REPL\n0.1 pre-alpha"

repl :: IO ()
repl
    =   putStrLn info
    >>  mapM lookupEnv ["LANG", "LC_ALL", "LC_CTYPE"]
    >>= return . any (isInfixOf "UTF" . map toUpper) . catMaybes
    >>= rec phagelude
    where
        rec :: SymTab PhageVal -> Bool -> IO ()
        rec tab unicode
            =   putStr ((if unicode then "\x03BB:" else ">") <> " ")
            >>  hFlush stdout
            >>  getLine
            >>= run True tab "stdin"
            >>= \newtab -> rec newtab unicode

main :: IO ()
main = getArgs >>= \args -> case args of
        (b : xs) -> readFile b >>= run False phagelude "stdin" >> return ()
        _ -> repl
