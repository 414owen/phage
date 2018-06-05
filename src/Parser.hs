module Parser
    ( parseAst
    ) where

import Val
import Data.Char
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr

syms :: String
syms = "'£€_<>[]`;{}¬:!#$%&*+./<=>?@\\^|-~"

sym :: Parser Char
sym = oneOf syms <?> "symbol"

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockCommentNested "/*" "*/"

symbol :: String -> Parser String
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

atom :: Parser PhageVal
atom = PAtom <$> lexeme ((:) <$> (lowerChar <|> sym) <*> many restChar)
    where
    restChar = lowerChar <|> upperChar <|> sym <|> digitChar

phstring :: Parser PhageVal
phstring = PQList <$> lexeme (char '"' >> manyTill (PChar <$> L.charLiteral) (char '"'))

number :: Parser PhageVal
number = PNum <$> lexeme L.decimal

val :: Parser PhageVal
val = list <|> phstring <|> number <|> atom

list :: Parser PhageVal
list = parens $ PList <$> many val

parseAst :: Parser [PhageVal]
parseAst = (sc >> many val) <* eof
