module Parser 
    ( parseAst
    ) where

import Ast
import Data.Char
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "// "
    blockCmnt = L.skipBlockCommentNested "/*" "*/"

symbol :: String -> Parser String
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

atom :: Parser AstNode
atom = Atom <$> (lexeme $
    ((:) <$> (letterChar <|> symbolChar) <*>
    many (satisfy restChar)))
    where
    restChar a = isPrint a && not (isSpace a) && not (a `elem` "()")

{- atom :: Parser AstNode -}
{- atom = lexeme $ -}
{-     fmap Atom <$> fmap (:) (letterChar <|> symbolChar) <*> -}
{-         many (brac <*> (alphaNumChar <|> symbolChar)) -}
{-         where -}
{-             brac = (oneOf "()") -}

number :: Parser AstNode
number = Number <$> lexeme L.decimal

val :: Parser AstNode
val = list <|> atom <|> number

list :: Parser AstNode
list = parens $ List <$> many val

parseAst :: Parser Ast
parseAst = Ast <$> many list
