{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad      (void)
import qualified Data.Text.Lazy     as T
import qualified Data.Text.Lazy.IO  as T
import           System.Directory   (doesFileExist)
import           System.Environment (getArgs)
import           Text.Megaparsec
import           Text.Printf

import           Lexer
import           Parser
import           Syntax

main :: IO ()
main = do
  args <- getArgs
  case args of
    []     -> putStrLn "Error - no source file specified."
    [path] -> do
      let path' = withExt "asm" path
      exists <- doesFileExist path'
      if exists
        then do
          src <- T.readFile path'
          case parse parseProgram "<stdin>" src of
            Left err -> print err
            Right ast -> do
              let out = concatMap (printf "%016b\n") (fromInstruction <$> ast)
              writeFile (asm2hack path) out
        else putStrLn "Error - source file does not exist."
    _  -> putStrLn "Error - too many command line arguments."

-- | Add an extension to a 'FilePath' if it does not alread have one.
withExt :: FilePath -> FilePath -> FilePath
withExt ext base
  | '.' `elem` base = base
  | otherwise       = base ++ "." ++ ext

asm2hack :: FilePath -> FilePath
asm2hack = reverse . ("kcah" ++) . drop 3 . reverse . withExt "asm"
