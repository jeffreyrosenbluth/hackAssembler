module Syntax where

import           Data.Word

data SInstruction
  = Loop String
  | Instr (Instruction AInstruction)
  deriving Show

data Instruction a
  = AInstr a
  | CInstr CInstruction
  deriving Show

fromInstruction :: Instruction Word16 -> Word16
fromInstruction (AInstr a) = a
fromInstruction (CInstr c) = fromCinstruction c

data AInstruction
  = Immediate Word16
  | Symbol String
  deriving Show

data CInstruction = CInstruction
  { comp :: Comp
  , dest :: Dest
  , jump :: Jump
  } deriving Show

fromCinstruction :: CInstruction -> Word16
fromCinstruction (CInstruction c d j) = fromComp c + fromDest d + fromJump j

data AM = A | M deriving Show

data Comp
  = Zero
  | One
  | NegOne
  | D
  | L AM
  | NotD
  | NotL AM
  | NegD
  | NegL AM
  | Dplus1
  | Lplus1 AM
  | Dminus1
  | Lminus1 AM
  | DplusL AM
  | DminusL AM
  | LminusD AM
  | DandL AM
  | DorL AM
  deriving Show

fromComp :: Comp -> Word16
fromComp Zero        = 60032
fromComp One         = 61376
fromComp NegOne      = 61056
fromComp D           = 58112
fromComp (L A)       = 60416
fromComp (L M)       = 64512
fromComp NotD        = 58716
fromComp (NotL A)    = 60480
fromComp (NotL M)    = 64576
fromComp NegD        = 58304
fromComp (NegL A)    = 60608
fromComp (NegL M)    = 64704
fromComp Dplus1      = 59328
fromComp (Lplus1 A)  = 60864
fromComp (Lplus1 M)  = 64960
fromComp Dminus1     = 58240
fromComp (Lminus1 A) = 60544
fromComp (Lminus1 M) = 64640
fromComp (DplusL A)  = 57472
fromComp (DplusL M)  = 61568
fromComp (DminusL A) = 58560
fromComp (DminusL M) = 62656
fromComp (LminusD A) = 57792
fromComp (LminusD M) = 61888
fromComp (DandL A)   = 57344
fromComp (DandL M)   = 61440
fromComp (DorL A)    = 58688
fromComp (DorL M)    = 62784

data Dest
  = None
  | MM
  | DD
  | MD
  | AA
  | AM
  | AD
  | AMD
  deriving Show

fromDest :: Dest -> Word16
fromDest None = 0
fromDest MM   = 8
fromDest DD   = 16
fromDest MD   = 24
fromDest AA   = 32
fromDest AM   = 40
fromDest AD   = 48
fromDest AMD  = 56

data Jump
  = Null
  | JGT
  | JEQ
  | JGE
  | JLT
  | JNE
  | JLE
  | JMP
  deriving Show

fromJump :: Jump -> Word16
fromJump Null = 0
fromJump JGT  = 1
fromJump JEQ  = 2
fromJump JGE  = 3
fromJump JLT  = 4
fromJump JNE  = 5
fromJump JLE  = 6
fromJump JMP  = 7
