{-|
Module      : Lion.Instruction
Description : RISC-V ISA
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com
-}

module Lion.Instruction where

import Clash.Prelude
import Data.Function ( on )

data Exception = IllegalInstruction
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

-- | Writeback pipeline instruction
data WbInstr = WbRegWr (Unsigned 5) (BitVector 32)
             | WbLoad Load (Unsigned 5) (BitVector 4)
             | WbStore
             | WbNop
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

-- | Memory pipeline instruction
data MeInstr = MeRegWr      (Unsigned 5)
             | MeStore                   (BitVector 32) (BitVector 4) (BitVector 32)
             | MeLoad  Load (Unsigned 5) (BitVector 32) (BitVector 4)
             | MeNop
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

-- | Execute pipeline instruction
data ExInstr = Ex       ExOp   (Unsigned 5) (BitVector 32)
             | ExBranch Branch              (BitVector 32)
             | ExStore  Store               (BitVector 32)
             | ExLoad   Load   (Unsigned 5) (BitVector 32)
             | ExAlu    Op     (Unsigned 5)
             | ExAluImm Op     (Unsigned 5) (BitVector 32)
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

-- | ALU operation
data Op = Add
        | Sub
        | Sll
        | Slt
        | Sltu
        | Xor
        | Srl
        | Sra
        | Or
        | And
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

-- | Branch operation
data Branch = Beq
            | Bne
            | Blt
            | Bge
            | Bltu
            | Bgeu
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

-- | branch calculation
branch :: Branch -> BitVector 32 -> BitVector 32 -> Bool
branch = \case
  Beq  -> not ... (/=)
  Bne  -> (/=)
  Bge  -> not ... (<) `on` sign
  Bgeu -> not ... (<)
  Blt  -> (<) `on` sign
  Bltu -> (<)
  where
    (...) = (.).(.)
    sign :: BitVector 32 -> Signed 32
    sign = unpack

data ExOp = Lui
          | Auipc
          | Jal
          | Jalr
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

-- | Store operation
data Store = Sb
           | Sh
           | Sw
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

-- | Load operation
data Load = Lb
          | Lh
          | Lw
          | Lbu
          | Lhu
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

parseInstr :: BitVector 32 -> Either Exception ExInstr
parseInstr i = case i of
  $(bitPattern ".........................0110111") -> Right $ Ex Lui   rd immU -- lui
  $(bitPattern ".........................0010111") -> Right $ Ex Auipc rd immU -- auipc
  $(bitPattern ".........................1101111") -> Right $ Ex Jal   rd immJ -- jal
  $(bitPattern ".................000.....1100111") -> Right $ Ex Jalr  rd immI -- jalr
  $(bitPattern ".................000.....1100011") -> Right $ ExBranch Beq  immB -- beq
  $(bitPattern ".................001.....1100011") -> Right $ ExBranch Bne  immB -- bne
  $(bitPattern ".................100.....1100011") -> Right $ ExBranch Blt  immB -- blt
  $(bitPattern ".................101.....1100011") -> Right $ ExBranch Bge  immB -- bge
  $(bitPattern ".................110.....1100011") -> Right $ ExBranch Bltu immB -- bltu
  $(bitPattern ".................111.....1100011") -> Right $ ExBranch Bgeu immB -- bgeu
  $(bitPattern ".................000.....0000011") -> Right $ ExLoad Lb  rd immI -- lb
  $(bitPattern ".................001.....0000011") -> Right $ ExLoad Lh  rd immI -- lh
  $(bitPattern ".................010.....0000011") -> Right $ ExLoad Lw  rd immI -- lw
  $(bitPattern ".................100.....0000011") -> Right $ ExLoad Lbu rd immI -- lbu
  $(bitPattern ".................101.....0000011") -> Right $ ExLoad Lhu rd immI -- lhu
  $(bitPattern ".................000.....0100011") -> Right $ ExStore Sb immS -- sb
  $(bitPattern ".................001.....0100011") -> Right $ ExStore Sh immS -- sh
  $(bitPattern ".................010.....0100011") -> Right $ ExStore Sw immS -- sw
  $(bitPattern ".................000.....0010011") -> Right $ ExAluImm Add  rd immI -- addi
  $(bitPattern ".................010.....0010011") -> Right $ ExAluImm Slt  rd immI -- slti
  $(bitPattern ".................011.....0010011") -> Right $ ExAluImm Sltu rd immI -- sltiu
  $(bitPattern ".................100.....0010011") -> Right $ ExAluImm Xor  rd immI -- xori
  $(bitPattern ".................110.....0010011") -> Right $ ExAluImm Or   rd immI -- ori
  $(bitPattern ".................111.....0010011") -> Right $ ExAluImm And  rd immI -- andi
  $(bitPattern "0000000..........001.....0010011") -> Right $ ExAluImm Sll  rd immI -- slli
  $(bitPattern "0000000..........101.....0010011") -> Right $ ExAluImm Srl  rd immI -- srli
  $(bitPattern "0100000..........101.....0010011") -> Right $ ExAluImm Sra  rd immI -- srai
  $(bitPattern "0000000..........000.....0110011") -> Right $ ExAlu Add  rd -- add
  $(bitPattern "0100000..........000.....0110011") -> Right $ ExAlu Sub  rd -- sub
  $(bitPattern "0000000..........001.....0110011") -> Right $ ExAlu Sll  rd -- sll
  $(bitPattern "0000000..........010.....0110011") -> Right $ ExAlu Slt  rd -- slt
  $(bitPattern "0000000..........011.....0110011") -> Right $ ExAlu Sltu rd -- sltu
  $(bitPattern "0000000..........100.....0110011") -> Right $ ExAlu Xor  rd -- xor
  $(bitPattern "0000000..........101.....0110011") -> Right $ ExAlu Srl  rd -- srl
  $(bitPattern "0100000..........101.....0110011") -> Right $ ExAlu Sra  rd -- sra
  $(bitPattern "0000000..........110.....0110011") -> Right $ ExAlu Or   rd -- or
  $(bitPattern "0000000..........111.....0110011") -> Right $ ExAlu And  rd -- and
  _ -> Left IllegalInstruction
  where
    rd = sliceRd i

    immI :: BitVector 32
    immI = signExtend $ slice d31 d20 i
    
    immS :: BitVector 32
    immS = signExtend $ slice d31 d25 i ++# slice d11 d7 i

    immB :: BitVector 32
    immB = signExtend (slice d31 d31 i ++# slice d7 d7 i ++# slice d30 d25 i ++# slice d11 d8 i) `shiftL` 1

    immU :: BitVector 32
    immU = (slice d31 d12 i) ++# 0
    
    immJ :: BitVector 32
    immJ = signExtend (slice d31 d31 i ++# slice d19 d12 i ++# slice d20 d20 i ++# slice d30 d25 i ++# slice d24 d21 i) `shiftL` 1


sliceRd :: BitVector 32 -> Unsigned 5
sliceRd = unpack . slice d11 d7

sliceRs1 :: BitVector 32 -> Unsigned 5
sliceRs1 = unpack . slice d19 d15

sliceRs2 :: BitVector 32 -> Unsigned 5
sliceRs2 = unpack . slice d24 d20

sliceOpcode :: BitVector 32 -> BitVector 7
sliceOpcode = slice d6 d0

sliceFunct3 :: BitVector 32 -> BitVector 3
sliceFunct3 = slice d14 d12

sliceFunct7 :: BitVector 32 -> BitVector 7
sliceFunct7 = slice d31 d25
