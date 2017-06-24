module Lexer  where

import           Control.Applicative
import           Control.Monad             (void)
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer     as L
import           Text.Megaparsec.Text.Lazy

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt empty
  where lineCmnt  = L.skipLineComment "//"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- | 'integer' parses an integer.
integer :: Parser Integer
integer = lexeme L.integer

-- | 'parens' parses something between parenthesis.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

contents :: Parser a -> Parser a
contents p = sc *> p <* eof
