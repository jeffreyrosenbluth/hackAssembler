module Symbol where

import           Control.Monad.Reader
import           Data.Map (Map)
import qualified Data.Map as M

import           Data.Word
import           Syntax

type SymbolTable = Map String Word16

data Environment = Environment
  { address     :: Word16
  , symbolTable :: SymbolTable
  }

type CompileM = Reader Environment [Instruction Word16]

predefinedSymbols :: SymbolTable
predefinedSymbols = M.fromList
  [ ("SP", 0)
  , ("LCL", 1)
  , ("ARG", 2)
  , ("THIS", 3)
  , ("THAT", 4)
  , ("R0", 0)
  , ("R1", 1)
  , ("R2", 2)
  , ("R3", 3)
  , ("R4", 4)
  , ("R5", 5)
  , ("R6", 6)
  , ("R7", 7)
  , ("R8", 8)
  , ("R9", 9)
  , ("R10", 10)
  , ("R11", 11)
  , ("R12", 12)
  , ("R13", 13)
  , ("R14", 14)
  , ("R15", 15)
  , ("SCREEN", 16384)
  , ("KBD", 24576)
  ]

label :: Word16 -> [SInstruction] -> SymbolTable -> SymbolTable
label _ [] st             = st
label n (Loop s : xs) st  = label n xs (M.insert s n st)
label n (Instr i : xs) st = label (n+1) xs st


compile :: [SInstruction] -> CompileM
compile [] = pure []
compile (Loop s : xs) = compile xs
compile (Instr (CInstr c) : xs) = fmap (CInstr c : ) (compile xs)
compile (Instr (AInstr (Immediate w)) : xs) = fmap (AInstr w : ) (compile xs)
compile (Instr (AInstr (Symbol s)) : xs) = do
  Environment addr table <- ask
  let mAddr = M.lookup s table
  case mAddr of
    Nothing ->
      local (\(Environment a t) -> Environment (a+1) (M.insert s a t))
            (fmap (AInstr addr : ) (compile xs))
    Just i -> fmap (AInstr i :) (compile xs)

runCompiler :: [SInstruction] -> [Instruction Word16]
runCompiler xs = runReader (compile xs) (Environment 16 symbols)
  where
    symbols = label 0 xs predefinedSymbols
