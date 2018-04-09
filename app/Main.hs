module Main where

import Ast
import Parser
import Interpreter
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

display :: String -> String -> IO ()
display s i = case parse parseAst s i of
    Left err ->
           putStrLn "Parse error:"
        >> print err
    Right ast -> case ast of
        (Ast []) -> return ()
        (Ast [node]) ->
            runExceptT (interpret ast)
            >>= \res ->
                case res of
                    Right val -> putStr ">> " >> print val
                    Left err -> putStrLn ("Phage Error:\n" <> err)
        _ ->
                putStrLn "The REPL only supports one expression at a time"
            >>  putStrLn ("Maybe you meant: '(" ++ i ++ ")'")
            >>  return ()

info = "The Phage Programming Language REPL\n0.1 pre-alpha"

repl :: IO ()
repl
    =   putStrLn info
    >>  mapM lookupEnv ["LANG", "LC_ALL", "LC_CTYPE"]
    >>= return . any (isInfixOf "UTF" . map toUpper) . catMaybes
    >>= rec
    where
        rec :: Bool -> IO ()
        rec unicode
            =   putStr ((if unicode then "\x03BB:" else ">") ++ " ")
            >>  hFlush stdout
            >>  getLine
            >>= display "stdin"
            >>  rec unicode

main :: IO ()
main = getArgs
    >>= \args ->
        case args of
            (a : b : xs) -> readFile b >>= display b
            _ -> repl
