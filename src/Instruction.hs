module Instruction where

import Clash.Prelude

data Exception = IllegalInstruction
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

newtype Instr = Instr { unInstr :: BitVector 32 }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX, BitPack)

parseAdd :: BitVector 32 -> Either Exception Instr
parseAdd i = case i of
  $(bitPattern "0000000..........000.....0110011") -> Right $ Instr i
  _ -> Left IllegalInstruction

rd :: Instr -> BitVector 5
rd = slice d11 d7 . pack 

rs1 :: Instr -> BitVector 5
rs1 = slice d19 d15 . pack

rs2 :: Instr -> BitVector 5
rs2 = slice d24 d20 . pack

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
