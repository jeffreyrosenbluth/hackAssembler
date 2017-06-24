{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Data.Text.Lazy            (Text)
import           Text.Megaparsec
import           Text.Megaparsec.Text.Lazy (Parser)

import           Lexer
import           Syntax

instruction :: Parser Instruction
instruction = AInstr <$> aInstruction <|> CInstr <$> cInstruction

aInstruction :: Parser AInstruction
aInstruction = fromInteger <$> (symbol "@" *> integer)

cInstruction :: Parser CInstruction
cInstruction = do
  d <- parseDest
  c <- parseComp
  j <- parseJump
  pure $ CInstruction c d j

parseDest :: Parser Dest
parseDest = option None $ try
          $ AMD <$ symEq "AMD"
        <|> MD  <$ symEq "MD"
        <|> DD  <$ symEq "D"
        <|> MM  <$ symEq "M"
        <|> AM  <$ symEq "AM"
        <|> AD  <$ symEq "AD"
        <|> AA  <$ symEq "A"
  where
    symEq str = symbol str <* symbol "="

parseComp :: Parser Comp
parseComp = Zero <$ symbol "0"
        <|> One  <$ symbol "1"
        <|> NegOne <$ symbol "-1"
        <|> try (NotD <$ symbol "!D")
        <|> try (NotL A <$ symbol "!A")
        <|> try (NotL M <$ symbol "!M")
        <|> try (NegD <$ symbol "-D")
        <|> try (NegL A <$ symbol "-A")
        <|> try (NegL M <$ symbol "-M")
        <|> try (Dplus1 <$ symbol "D" <* symbol "+" <* symbol "1")
        <|> try (Lplus1 A <$ symbol "A" <* symbol "+" <* symbol "1")
        <|> try (Lplus1 M <$ symbol "M" <* symbol "+" <* symbol "1")
        <|> try (Dminus1 <$ symbol "D" <* symbol "-" <* symbol "1")
        <|> try (Lminus1 A <$ symbol "A" <* symbol "-" <* symbol "1")
        <|> try (Lminus1 M <$ symbol "M" <* symbol "-" <* symbol "1")
        <|> try (DplusL A <$ symbol "D" <* symbol "+" <* symbol "A")
        <|> try (DplusL M <$ symbol "D" <* symbol "+" <* symbol "M")
        <|> try (LminusD M <$ symbol "M" <* symbol "-" <* symbol "D")
        <|> try (LminusD A <$ symbol "A" <* symbol "-" <* symbol "D")
        <|> try (DminusL A <$ symbol "D" <* symbol "-" <* symbol "A")
        <|> try (DminusL M <$ symbol "D" <* symbol "-" <* symbol "M")
        <|> try (DandL A <$ symbol "D" <* symbol "&" <* symbol "A")
        <|> try (DandL M <$ symbol "D" <* symbol "&" <* symbol "M")
        <|> try (DorL A <$ symbol "D" <* symbol "|" <* symbol "A")
        <|> try (DorL M <$ symbol "D" <* symbol "|" <* symbol "M")
        <|> try (D <$ symbol "D")
        <|> try (L M <$ symbol "M")
        <|> try (L A <$ symbol "A")

parseJump :: Parser Jump
parseJump = option Null
          $ try (JGT <$ semiSym "JGT")
        <|> try (JEQ <$ semiSym "JEQ")
        <|> try (JGE <$ semiSym "JGE")
        <|> try (JLT <$ semiSym "JLT")
        <|> try (JNE <$ semiSym "JNE")
        <|> try (JLE <$ semiSym "JLE")
        <|> try (JMP <$ semiSym "JMP")
  where
    semiSym str = symbol ";" *> symbol str

parseProgram :: Parser [Instruction]
parseProgram = contents $ some instruction
