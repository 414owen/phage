module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Core
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Interpreter
import Parser
import System.Console.Haskeline
import System.Environment
import System.IO
import Text.Megaparsec
import Val

source :: [String] -> IO (String, String)
source (_:file:_) = (file,) <$> readFile file
source _ = ("stdin",) <$> getContents

getAst :: String -> String -> Either String [PhageVal]
getAst fname src = case parse parseAst fname src of
    Left err -> Left $ "Parse error:\n" <> show err <> "\n"
    Right ast -> case ast of
        [] -> Left ""
        _ -> Right ast

run :: Bool -> SymTab -> String -> String -> IO SymTab
run repl tab fname source = case (repl, getAst fname source) of
    (_, Left e) -> const tab <$> putStr e
    (True, Right (x : y : xs)) -> const tab <$> putStrLn
        "The repl only accepts one expression at a time"
    (_, Right ast) -> runExceptT (lastBlock tab ast)
        >>= \res -> case res of
            Right (eds, val) ->
                const (newTabM eds tab) <$> (putStr $ if repl
                    then "\n>> " <> show val <> "\n"
                    else "")
            Left err ->
                const tab <$> putStrLn ("Phage Error:\n" <> err)

info = "The Phage Programming Language REPL\n0.1 pre-alpha"

prelFile = "prelude/prelude.scm"

repl :: IO ()
repl =  putStrLn info
    >>  readFile prelFile
    >>= run False core prelFile
    >>= \tab -> mapM lookupEnv ["LANG", "LC_ALL", "LC_CTYPE"]
    >>= return . any (isInfixOf "UTF" . map toUpper) . catMaybes
    >>= runInputT defaultSettings . rec tab
    where
        rec :: SymTab -> Bool -> InputT IO ()
        rec tab unicode
            =   getInputLine ((if unicode then "\x03BB:" else ">") <> " ")
            >>= \line -> lift (case line of
                Nothing -> pure tab
                Just line -> run True tab "stdin" line)
            >>= \newTab -> rec newTab unicode

main :: IO ()
main = getArgs >>= \args -> case args of
        (b : xs) -> readFile b
            >>= run False core b >> pure ()
        _ -> repl
