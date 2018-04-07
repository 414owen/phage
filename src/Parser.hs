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
atom = AAtom <$> (lexeme $
    ((:) <$> (letterChar <|> symbolChar) <*>
    many (satisfy restChar)))
    where
    restChar a = isPrint a && not (isSpace a) && not (a `elem` "()")

number :: Parser AstNode
number = ANum <$> lexeme L.decimal

val :: Parser AstNode
val = list <|> number <|> atom

list :: Parser AstNode
list = parens $ AList <$> many val

parseAst :: Parser Ast
parseAst = Ast <$> many list <* eof
