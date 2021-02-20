module Instruction where

import Clash.Prelude
import Data.Function ( on )

data Exception = IllegalInstruction
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

data WbInstr = WbRegWr (Unsigned 5) (BitVector 32)
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

data MeInstr = MeRegWr (Unsigned 5) (BitVector 32)
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

data ExInstr = Ex ExOp (Unsigned 5) (BitVector 32)
--             | ExBranch Branch (BitVector 32)
             | ExAlu Op (Unsigned 5)
             | ExAluImm Op (Unsigned 5) (BitVector 32)
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

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

alu :: Op -> BitVector 32 -> BitVector 32 -> BitVector 32
alu = \case
  Add  -> (+)
  Sub  -> (-)
  Sll  -> \x y -> x `shiftL` shamt y
  Slt  -> boolToBV ... (<) `on` sign
  Sltu -> boolToBV ... (<)
  Xor  -> xor
  Srl  -> \x y -> x `shiftR` shamt y
  Sra  -> \x y -> pack $ sign x `shiftR` shamt y
  Or   -> (.|.)
  And  -> (.&.)
  where
    shamt = unpack . resize . slice d4 d0
    sign = unpack :: BitVector 32 -> Signed 32
    (...) = (.).(.)

data Branch = Beq
            | Bne
            | Blt
            | Bge
            | Bltu
            | Bgeu
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

branch :: Branch -> BitVector 32 -> BitVector 32 -> Bool
branch = \case
  Beq  -> (==)
  Bne  -> (/=)
  Blt  -> (<)  `on` sign
  Bge  -> (>=) `on` sign
  Bltu -> (<)
  Bgeu -> (>=)
  where
    sign :: BitVector 32 -> Signed 32
    sign = unpack

data ExOp = Lui
          | Auipc
          | Jal
          | Jalr
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

parseInstr :: BitVector 32 -> Either Exception ExInstr
parseInstr i = case i of
  $(bitPattern ".........................0110111") -> Right $ Ex Lui   rd immU -- lui
  $(bitPattern ".........................0010111") -> Right $ Ex Auipc rd immU -- auipc
  $(bitPattern ".........................1101111") -> Right $ Ex Jal   rd immJ -- jal
  $(bitPattern ".................000.....1100111") -> Right $ Ex Jalr  rd immI -- jalr
--  $(bitPattern ".................000.....1100011") -> Right $ ExBranch Beq  immB -- beq
--  $(bitPattern ".................001.....1100011") -> Right $ ExBranch Bne  immB -- bne
--  $(bitPattern ".................100.....1100011") -> Right $ ExBranch Blt  immB -- blt
--  $(bitPattern ".................101.....1100011") -> Right $ ExBranch Bge  immB -- bge
--  $(bitPattern ".................110.....1100011") -> Right $ ExBranch Bltu immB -- bltu
--  $(bitPattern ".................111.....1100011") -> Right $ ExBranch Bgeu immB -- bgeu
--  $(bitPattern ".................000.....0000011") -> Right $ ExLoad Lb  rd immI -- lb
--  $(bitPattern ".................001.....0000011") -> Right $ ExLoad Lh  rd immI -- lh
--  $(bitPattern ".................010.....0000011") -> Right $ ExLoad Lw  rd immI -- lw
--  $(bitPattern ".................100.....0000011") -> Right $ ExLoad Lbu rd immI -- lbu
--  $(bitPattern ".................101.....0000011") -> Right $ ExLoad Lhu rd immI -- lhu
--  $(bitPattern ".................000.....0100011") -> Right $ ExStore Sb immS -- sb
--  $(bitPattern ".................001.....0100011") -> Right $ ExStore Sh immS -- sh
--  $(bitPattern ".................010.....0100011") -> Right $ ExStore Sw immS -- sw
  $(bitPattern ".................000.....0010011") -> Right $ ExAluImm Add  rd immI -- addi
  $(bitPattern ".................010.....0010011") -> Right $ ExAluImm Slt  rd immI -- slti
  $(bitPattern ".................011.....0010011") -> Right $ ExAluImm Sltu rd immI -- sltiu
  $(bitPattern ".................100.....0010011") -> Right $ ExAluImm Xor  rd immI -- xori
  $(bitPattern ".................110.....0010011") -> Right $ ExAluImm Or   rd immI -- ori
  $(bitPattern ".................111.....0010011") -> Right $ ExAluImm And  rd immI -- andi
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

{-
data Type = R
          | I
          | S
          | B
          | U
          | J
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

data Opcode = Load
            | Store
            | Branch
            | Jalr
            | Jal
            | Opimm
            | Op
            | Auipc
            | Lui
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

data Instr n (t :: Type) (op :: Opcode) where
  Add :: Word n -> Instr n 'R 'Op
  Addi :: Word n -> Instr n 'I 'Opimm
  Beq :: Word n -> Instr n 'B 'Branch

class Alu n t op where
  alu :: Instr n t op -> BitVector n -> BitVector n -> BitVector n

instance KnownNat n => Alu n 'R 'Op where
  alu (Add _) = (+)

instance KnownNat n => Alu n 'I 'Opimm where
  alu (Addi _) = (+)

class Monad m => Execute n t op m where
  execute :: Instr n t op -> m () 

instance (KnownNat n, Alu n t op, Monad m) => Execute n t op m where
  execute i = do
    let x = alu i 2 3 
    return ()

instance Monad m => Execute n t 'Branch m where
  execute i = return ()
-}
{-
data Type = R | I | S | B | U | J
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

newtype Instr opcode = Instr { unInstr :: BitVector 32 }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

type family InstrType (op :: Opcode) :: Type where
  InstrType 'Load   = 'I
  InstrType 'Store  = 'S
  InstrType 'Branch = 'B 
  InstrType 'Jalr   = 'I
  InstrType 'Jal    = 'J
  InstrType 'Opimm  = 'I
  InstrType 'Op     = 'R
  InstrType 'Auipc  = 'U
  InstrType 'Lui    = 'U

data Opcode = Load
            | Store
            | Branch
            | Jalr
            | Jal
            | Opimm
            | Op
            | Auipc
            | Lui
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

fromOpcode :: Opcode -> BitVector 7
fromOpcode = \case
  Load   -> 0b0000011
  Store  -> 0b0100011
  Branch -> 0b1100011
  Jalr   -> 0b1100111
  Jal    -> 0b1101111
  Opimm  -> 0b0010011
  Op     -> 0b0110011
  Auipc  -> 0b0010111
  Lui    -> 0b0110111

parseOpcode :: BitVector 7 -> Either Exception Opcode
parseOpcode = \case
  $(bitPattern "0000011") -> Right Load
  $(bitPattern "0100011") -> Right Store
  $(bitPattern "1100011") -> Right Branch
  $(bitPattern "1100111") -> Right Jalr
  $(bitPattern "1101111") -> Right Jal
  $(bitPattern "0010011") -> Right Opimm
  $(bitPattern "0110011") -> Right Op
  $(bitPattern "0010111") -> Right Auipc
  $(bitPattern "0110111") -> Right Lui
  _                       -> Left IllegalInstruction

data Branch = Beq
            | Bne
            | Blt
            | Bge
            | Bltu
            | Bgeu
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
-}
{-
data InstrType = R | I
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

newtype Instr (t :: InstrType) = Instr { unInstr :: BitVector 32 }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

data Instr'  (t :: InstrType) where
  Load :: BitVector 32 -> Instr' 'I
deriving instance Generic (Instr' 'I)  

class Rd (t :: InstrType) where
  rd :: Instr t -> BitVector 7
  rd = slice d6 d0 . unInstr

instance Rd 'R
instance Rd 'I

--mkInstr :: BitVector 32 -> Instr (t :: InstrType)
--mkInstr 0 = Instr 0 :: Instr 'R
--mkInstr _ = Instr 1 :: Instr 'I
-}
{-
class Funct3 i where
  funct3 :: i -> BitVector 3
  funct3 = slice d14 d12

instance InstrType op ~ R => Funct3 (Instr op)
instance InstrType op ~ I => Funct3 (Instr op)
instance InstrType op ~ S => Funct3 (Instr op)
instance InstrType op ~ B => Funct3 (Instr op)

class Rs1 i where
  rs1 :: i -> BitVector 5
  rs1 = slice d19 d15

instance InstrType op ~ R => Rs1 (Instr op)
instance InstrType op ~ I => Rs1 (Instr op)
instance InstrType op ~ S => Rs1 (Instr op)
instance InstrType op ~ B => Rs1 (Instr op)

class Rs2 i where
  rs2 :: i -> BitVector 5
  rs2 = slice d24 d20

instance InstrType op ~ R => Rs2 (Instr op)
instance InstrType op ~ S => Rs2 (Instr op)
instance InstrType op ~ B => Rs2 (Instr op)
-}
{-
class Imm i where
  imm :: i -> BitVector 32

instance Imm (Instr I) where
  imm = _

instance Imm (Instr S) where
  imm = _

instance Imm (Instr B) where
  imm = _

instance Imm (Instr U) where
  imm = _

instance Imm (Instr J) where
  imm = _
-}
{-  

newtype Instr = Instr { unInstr :: BitVector 32 }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX, BitPack)
-}
{-
data Type i = R i
            | I i
            | S i
            | B i
            | U i
            | J i
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX, BitPack)
-}
{-
class Opcode i where
  opcode :: i -> BitVector 7
  
instance Opcode Instr where
  opcode = slice d6 d0 . pack

class (Opcode i, Rd i, Funct3 i, Rs1 i, Rs2 i) => R i where
  funct7 :: i -> BitVector 7

class (Opcode i, Rd i, Funct3 i, Rs1 i) => I i where
  iImm :: i -> BitVector 32


class Rd i where
  rd :: BitVector 32 -> BitVector 5
  rd = slice d11 d7

class Instruction i => Funct3 i where
  funct3 :: BitVector 32 -> BitVector 3
  funct3 = slice d14 d12

class Instruction i => Rs1 i where
  rs1 :: BitVector 32 -> BitVector 5
  rs1 = slice d19 d15

class Instruction i => Rs2 i where
  rs2 :: BitVector 32 -> BitVector 5
  rs2 = slice d24 d20
-}
