{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-|
Module      : Lion.Instruction
Description : RISC-V ISA
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com
-}

module Lion.Instruction where

import Clash.Prelude
import Data.Either ( isLeft )
import Data.Function ( on )

import Lion.Util.Clash

data Exception = IllegalInstruction
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

-- | Writeback pipeline instruction
data WbInstr xl
    = WbRegWr (Unsigned 5) (BitVector xl) (OpWidth xl)
    | WbLoad Load (Unsigned 5) (BitVector (Div xl 8))
    | WbStore
    | WbNop
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

-- | Memory pipeline instruction
data MeInstr xl
    = MeRegWr      (Unsigned 5) (OpWidth xl)
    | MeJump       (Unsigned 5) (BitVector xl)
    | MeBranch
    | MeStore                   (BitVector xl) (BitVector (Div xl 8)) (BitVector xl)
    | MeLoad  Load (Unsigned 5) (BitVector xl) (BitVector (Div xl 8))
    | MeNop
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

-- | Execute pipeline instruction
data ExInstr xl
    = Ex       ExOp   (Unsigned 5) (BitVector xl)
    | ExJump   Jump   (Unsigned 5) (BitVector xl)
    | ExBranch Branch              (BitVector xl)
    | ExStore  Store               (BitVector xl)
    | ExLoad   Load   (Unsigned 5) (BitVector xl)
    | ExAlu    Op     (Unsigned 5)                (OpWidth xl)
    | ExAluImm Op     (Unsigned 5) (BitVector xl) (OpWidth xl)
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

data OpWidth xl where
    FullWidth :: OpWidth xl
    ShortWidth32 :: 33 <= xl => OpWidth xl

deriving instance Show (OpWidth xl)
deriving instance Eq (OpWidth xl)

instance NFDataX (OpWidth xl) where
    deepErrorX = errorX
    hasUndefined = isLeft . isX
    ensureSpine = id
    rnfX = rwhnfX

shorten :: forall xl . KnownNat xl => OpWidth xl -> BitVector xl -> BitVector xl
shorten = \ case
    FullWidth -> id
    ShortWidth32 -> bitCoerce . (signResize :: _ 32 -> _) . resize

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
branch :: KnownNat xl => Branch -> BitVector xl -> BitVector xl -> Bool
branch = \case
  Beq  -> not ... (/=)
  Bne  -> (/=)
  Bge  -> not ... (<) `on` sign
  Bgeu -> not ... (<)
  Blt  -> (<) `on` sign
  Bltu -> (<)
  where
    (...) = (.).(.)

data ExOp = Lui
          | Auipc
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

data Jump = Jal | Jalr
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

-- | Store operation
data Store = Sb
           | Sh
           | Sw
           | Sd
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

-- | Load operation
data Load = Lb
          | Lh
          | Lw
          | Lbu
          | Lhu
          | Lwu
          | Ld
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

parseInstr :: forall xl . KnownNat xl => BitVector 32 -> Either Exception (ExInstr xl)
parseInstr i = case i of
  $(bitPattern ".........................0110111") -> Right $ Ex Lui   rd immU -- lui
  $(bitPattern ".........................0010111") -> Right $ Ex Auipc rd immU -- auipc
  $(bitPattern ".........................1101111") -> Right $ ExJump Jal  rd immJ -- jal
  $(bitPattern ".................000.....1100111") -> Right $ ExJump Jalr rd immI -- jalr
  $(bitPattern ".................000.....1100011") -> Right $ ExBranch Beq  immB -- beq
  $(bitPattern ".................001.....1100011") -> Right $ ExBranch Bne  immB -- bne
  $(bitPattern ".................100.....1100011") -> Right $ ExBranch Blt  immB -- blt
  $(bitPattern ".................101.....1100011") -> Right $ ExBranch Bge  immB -- bge
  $(bitPattern ".................110.....1100011") -> Right $ ExBranch Bltu immB -- bltu
  $(bitPattern ".................111.....1100011") -> Right $ ExBranch Bgeu immB -- bgeu
  $(bitPattern ".................000.....0000011") -> Right $ ExLoad Lb  rd immI -- lb
  $(bitPattern ".................001.....0000011") -> Right $ ExLoad Lh  rd immI -- lh
  $(bitPattern ".................010.....0000011") -> Right $ ExLoad Lw  rd immI -- lw
  $(bitPattern ".................011.....0000011") -> Right $ ExLoad Ld  rd immI -- ld
  $(bitPattern ".................100.....0000011") -> Right $ ExLoad Lbu rd immI -- lbu
  $(bitPattern ".................101.....0000011") -> Right $ ExLoad Lhu rd immI -- lhu
  $(bitPattern ".................110.....0000011") -> Right $ ExLoad Lwu rd immI -- lwu
  $(bitPattern ".................000.....0100011") -> Right $ ExStore Sb immS -- sb
  $(bitPattern ".................001.....0100011") -> Right $ ExStore Sh immS -- sh
  $(bitPattern ".................010.....0100011") -> Right $ ExStore Sw immS -- sw
  $(bitPattern ".................011.....0100011") -> Right $ ExStore Sd immS -- sd
  $(bitPattern ".................000.....001.011") -> ExAluImm Add  rd immI <$> opWidth -- addi
  $(bitPattern ".................010.....0010011") -> Right $ ExAluImm Slt  rd immI FullWidth -- slti
  $(bitPattern ".................011.....0010011") -> Right $ ExAluImm Sltu rd immI FullWidth -- sltiu
  $(bitPattern ".................100.....0010011") -> Right $ ExAluImm Xor  rd immI FullWidth -- xori
  $(bitPattern ".................110.....0010011") -> Right $ ExAluImm Or   rd immI FullWidth -- ori
  $(bitPattern ".................111.....0010011") -> Right $ ExAluImm And  rd immI FullWidth -- andi
  $(bitPattern "000000x..........001.....001.011")
    | 0 == x || 32 < natVal immI                   -> ExAluImm Sll  rd immI <$> opWidth -- slli
  $(bitPattern "000000x..........101.....001.011")
    | 0 == x || 32 < natVal immI                   -> ExAluImm Srl  rd immI <$> opWidth -- srli
  $(bitPattern "010000x..........101.....001.011")
    | 0 == x || 32 < natVal immI                   -> ExAluImm Sra  rd immI <$> opWidth -- srai
  $(bitPattern "0000000..........000.....011.011") -> ExAlu Add  rd <$> opWidth -- add
  $(bitPattern "0100000..........000.....011.011") -> ExAlu Sub  rd <$> opWidth -- sub
  $(bitPattern "0000000..........001.....011.011") -> ExAlu Sll  rd <$> opWidth -- sll
  $(bitPattern "0000000..........010.....0110011") -> Right $ ExAlu Slt  rd FullWidth -- slt
  $(bitPattern "0000000..........011.....0110011") -> Right $ ExAlu Sltu rd FullWidth -- sltu
  $(bitPattern "0000000..........100.....0110011") -> Right $ ExAlu Xor  rd FullWidth -- xor
  $(bitPattern "0000000..........101.....011.011") -> ExAlu Srl  rd <$> opWidth -- srl
  $(bitPattern "0100000..........101.....011.011") -> ExAlu Sra  rd <$> opWidth -- sra
  $(bitPattern "0000000..........110.....0110011") -> Right $ ExAlu Or   rd FullWidth -- or
  $(bitPattern "0000000..........111.....0110011") -> Right $ ExAlu And  rd FullWidth -- and
  _ -> Left IllegalInstruction
  where
--    npcB = immB + pc
--    npcJ = immJ + pc

    rd = sliceRd i

    immI :: BitVector xl
    immI = signResize $ slice d31 d20 i
    
    immS :: BitVector xl
    immS = signResize $ slice d31 d25 i ++# slice d11 d7 i

    immB :: BitVector xl
    immB = signResize (slice d31 d31 i ++# slice d7 d7 i ++# slice d30 d25 i ++# slice d11 d8 i) `shiftL` 1

    immU :: BitVector xl
    immU = zeroResize (slice d31 d12 i ++# 0 :: BitVector 32)
    
    immJ :: BitVector xl
    immJ = signResize (slice d31 d31 i ++# slice d19 d12 i ++# slice d20 d20 i ++# slice d30 d25 i ++# slice d24 d21 i) `shiftL` 1

    opWidth
      | testBit i 3 = case compareSNat d33 (SNat :: SNat xl) of
            SNatLE -> pure ShortWidth32
            SNatGT -> Left IllegalInstruction
      | otherwise = pure FullWidth

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
