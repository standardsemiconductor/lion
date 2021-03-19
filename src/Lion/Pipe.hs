{-|
Module      : Lion.Pipe
Description : RISC-V 5-stage pipeline
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com
-}

module Lion.Pipe where

import Clash.Prelude
import Control.Lens hiding ( op )
import Control.Monad.RWS
import Data.Maybe ( isJust )
import Data.Monoid.Generic
import Lion.Instruction
import Lion.Rvfi

-- | Pipeline configuration
newtype PipeConfig = PipeConfig
  { startPC :: BitVector 32 -- ^ initial Program Counter address, default = 0
  }
  deriving stock (Generic, Show, Eq)

-- | Default pipeline configuration
defaultPipeConfig :: PipeConfig
defaultPipeConfig = PipeConfig 0

-- | Pipeline inputs
data ToPipe = ToPipe
  { _fromRs1 :: BitVector 32
  , _fromRs2 :: BitVector 32
  , _fromAlu :: BitVector 32
  , _fromMem :: BitVector 32
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''ToPipe

-- | Memory access - Lion has a shared instruction/memory bus
data MemoryAccess = InstrMem -- ^ instruction access
                  | DataMem  -- ^ data access
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

-- | Memory bus
data ToMem = ToMem
  { memAccess   :: MemoryAccess         -- ^ memory access type
  , memAddress  :: BitVector 32         -- ^ memory address
  , memByteMask :: BitVector 4          -- ^ memory byte mask
  , memWrite    :: Maybe (BitVector 32) -- ^ read=Nothing write=Just wr
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

-- | Construct instruction memory access
instrMem 
  :: BitVector 32 -- ^ instruction address
  -> ToMem
instrMem addr = ToMem
  { memAccess   = InstrMem
  , memAddress  = addr
  , memByteMask = 0xF
  , memWrite    = Nothing
  }

-- | Construct data memory access
dataMem 
  :: BitVector 32         -- ^ memory address
  -> BitVector 4          -- ^ byte mask
  -> Maybe (BitVector 32) -- ^ write
  -> ToMem
dataMem addr mask wrM = ToMem
  { memAccess   = DataMem
  , memAddress  = addr
  , memByteMask = mask
  , memWrite    = wrM
  }

-- | Pipeline outputs
data FromPipe = FromPipe
  { _toMem       :: First ToMem
  , _toRs1Addr   :: First (Unsigned 5)
  , _toRs2Addr   :: First (Unsigned 5)
  , _toRd        :: First (Unsigned 5, BitVector 32)
  , _toAluOp     :: First Op
  , _toAluInput1 :: First (BitVector 32)
  , _toAluInput2 :: First (BitVector 32)
  , _toRvfi      :: First Rvfi
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
  deriving Semigroup via GenericSemigroup FromPipe
  deriving Monoid via GenericMonoid FromPipe
makeLenses ''FromPipe

data Control = Control
  { _firstCycle  :: Bool                             -- ^ First cycle True, then always False
  , _exBranching :: Bool                             -- ^ execute stage branch/jump
  , _meBranching :: Maybe (BitVector 32)             -- ^ memory stage branch/jump
  , _deLoad      :: Bool                             -- ^ decode stage load
  , _exLoad      :: Bool                             -- ^ execute stage load
  , _meMemory    :: Bool                             -- ^ memory stage load/store
  , _wbMemory    :: Bool                             -- ^ writeback stage load/store
  , _meRegFwd    :: Maybe (Unsigned 5, BitVector 32) -- ^ memory stage register forwarding
  , _wbRegFwd    :: Maybe (Unsigned 5, BitVector 32) -- ^ writeback stage register forwading
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''Control

mkControl :: Control
mkControl = Control 
  { _firstCycle  = True   
  , _exBranching = False
  , _meBranching = Nothing
  , _deLoad      = False
  , _exLoad      = False
  , _meMemory    = False
  , _wbMemory    = False
  , _meRegFwd    = Nothing
  , _wbRegFwd    = Nothing
  }

data Pipe = Pipe
  { _fetchPC :: BitVector 32

  -- decode stage
  , _dePC    :: BitVector 32

  -- execute stage
  , _exIR    :: Maybe ExInstr
  , _exPC    :: BitVector 32
  , _exRs1   :: Unsigned 5
  , _exRs2   :: Unsigned 5
  , _exRvfi  :: Rvfi

  -- memory stage
  , _meIR    :: Maybe MeInstr
  , _meRvfi  :: Rvfi

  -- writeback stage
  , _wbIR    :: Maybe WbInstr
  , _wbNRet  :: BitVector 64
  , _wbRvfi  :: Rvfi

  -- pipeline control
  , _control :: Control
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''Pipe

mkPipe :: PipeConfig -> Pipe
mkPipe config = Pipe
  { _fetchPC = startPC config

  -- decode stage 
  , _dePC    = 0
  
  -- execute stage
  , _exIR    = Nothing
  , _exPC    = 0
  , _exRs1   = 0
  , _exRs2   = 0
  , _exRvfi  = mkRvfi

  -- memory stage
  , _meIR    = Nothing
  , _meRvfi  = mkRvfi
 
  -- writeback stage
  , _wbIR    = Nothing
  , _wbNRet  = 0
  , _wbRvfi  = mkRvfi
  
  -- pipeline control
  , _control = mkControl
  }

-- | 5-Stage RISC-V pipeline
pipe 
  :: HiddenClockResetEnable dom
  => PipeConfig
  -> Signal dom ToPipe
  -> Signal dom FromPipe
pipe config = mealy pipeMealy (mkPipe config)
  where
    pipeMealy s i = let ((), s', o) = runRWS pipeM i s
                    in (s', o) 

-- | reset control signals (except first cycle)
resetControl :: MonadState Pipe m => m ()
resetControl = do
  control.exBranching .= False
  control.meBranching .= Nothing
  control.deLoad      .= False
  control.exLoad      .= False
  control.meMemory    .= False
  control.wbMemory    .= False
  control.meRegFwd    .= Nothing
  control.wbRegFwd    .= Nothing

-- | Monadic pipeline
pipeM :: RWS ToPipe FromPipe Pipe ()
pipeM = do
  resetControl
  writeback
  memory
  execute
  decode
  fetch

-- | Writeback stage
writeback :: RWS ToPipe FromPipe Pipe ()
writeback = withInstr wbIR $ \instr -> do
  wbRvfi.rvfiValid .= True
  wbRvfi.rvfiOrder <~ wbNRet <<+= 1
  case instr of
    WbRegWr rdAddr wr -> do
      wbRvfi.rvfiRdAddr .= rdAddr
      rdData <- wbRvfi.rvfiRdWData <.= guardZero rdAddr wr
      scribe toRd . First =<< control.wbRegFwd <.= Just (rdAddr, rdData)
    WbLoad op rdAddr mask -> do
      control.wbMemory .= True
      wbRvfi.rvfiRdAddr .= rdAddr
      mem <- wbRvfi.rvfiMemRData <<~ view fromMem
      let wr = case op of
            Lb  -> signExtend $ sliceByte mask mem
            Lh  -> signExtend $ sliceHalf mask mem
            Lw  -> mem
            Lbu -> zeroExtend $ sliceByte mask mem
            Lhu -> zeroExtend $ sliceHalf mask mem
      rdData <- wbRvfi.rvfiRdWData <.= guardZero rdAddr wr
      scribe toRd . First =<< control.wbRegFwd <.= Just (rdAddr, rdData)
    WbStore -> control.wbMemory .= True
    WbNop -> return ()
  scribe toRvfi . First . Just =<< use wbRvfi
  where
    guardZero 0 = const 0
    guardZero _ = id

-- | Memory stage
memory :: RWS ToPipe FromPipe Pipe ()
memory = do
  wbIR   .= Nothing
  wbRvfi <~ use meRvfi
  withInstr meIR $ \case
    MeNop -> wbIR ?= WbNop
    MeRegWr rd -> do
      wr <- view fromAlu
      control.meRegFwd ?= (rd, wr)
      wbIR ?= WbRegWr rd wr
    MeJump jump rd pc4 -> do
      npc <- wbRvfi.rvfiPcWData <<~ views fromAlu (jumpAddress jump) -- jal: id, jalr: flip clearBit 0
      wbRvfi.rvfiTrap ||= isMisaligned npc
      control.meBranching ?= npc
      control.meRegFwd ?= (rd, pc4)
      wbIR ?= WbRegWr rd pc4
    MeBranch isBranch pc4 -> do
      npc <- wbRvfi.rvfiPcWData <<~ if isBranch
                                      then do
                                        branchPC <- view fromAlu
                                        control.meBranching <?= branchPC
                                      else return pc4
      wbRvfi.rvfiTrap ||= isMisaligned npc
      wbIR ?= WbNop
    MeStore addr mask value -> do
      control.meMemory .= True
      scribe toMem $ First $ Just $ dataMem addr mask $ Just value
      wbRvfi.rvfiMemAddr  .= addr
      wbRvfi.rvfiMemWMask .= mask
      wbRvfi.rvfiMemWData .= value
      wbIR ?= WbStore
    MeLoad op rdAddr addr mask -> do
      control.meMemory .= True
      scribe toMem $ First $ Just $ dataMem addr mask Nothing
      wbRvfi.rvfiMemAddr  .= addr
      wbRvfi.rvfiMemRMask .= mask
      wbIR ?= WbLoad op rdAddr mask

-- | Execute stage
execute :: RWS ToPipe FromPipe Pipe ()
execute = do
  meIR .= Nothing
  meRvfi <~ use exRvfi
  pc  <- meRvfi.rvfiPcRData <<~ use exPC
  pc4 <- meRvfi.rvfiPcWData <.= pc + 4
  rs1Data <- meRvfi.rvfiRs1Data <<~ regFwd exRs1 fromRs1 (control.meRegFwd) (control.wbRegFwd)
  rs2Data <- meRvfi.rvfiRs2Data <<~ regFwd exRs2 fromRs2 (control.meRegFwd) (control.wbRegFwd)
  withInstr exIR $ \case
    Ex op rd imm -> case op of
      Lui -> do 
        scribeAlu Add 0 imm
        meIR ?= MeRegWr rd
      Auipc -> do
        scribeAlu Add pc imm
        meIR ?= MeRegWr rd
    ExJump jump rd imm -> do
      control.exBranching .= True
      case jump of
        Jal -> do
--          scribeAlu Add pc imm -- compute jump address with alu
          meIR ?= MeJump Jal rd (pc + imm) pc4
        Jalr -> do
--          scribeAlu Add rs1Data imm -- compute jump address with alu
          meIR ?= MeJump Jalr rd (rs1Data + imm) pc4
    ExBranch op imm -> do
      let isBranch = branch op rs1Data rs2Data
      when isBranch $ control.exBranching .= True
--      scribeAlu Add pc imm -- compute branch address with alu
      meIR ?= MeBranch isBranch (pc + imm) pc4
    ExStore op imm -> do
      let addr = rs1Data + imm            -- unaligned
          addr' = addr .&. complement 0x3 -- aligned
      case op of
        Sb -> let wr = concatBitVector# $ replicate d4 $ slice d7 d0 rs2Data
              in meIR ?= MeStore addr' (byteMask addr) wr
        Sh -> do
          meRvfi.rvfiTrap ||= isMisalignedHalf addr -- trap on half-word boundary
          let wr = concatBitVector# $ replicate d2 $ slice d15 d0 rs2Data
          meIR ?= MeStore addr' (halfMask addr) wr
        Sw -> do
          meRvfi.rvfiTrap ||= isMisaligned addr -- trap on word boundary
          meIR ?= MeStore addr' 0xF rs2Data
    ExLoad op rdAddr imm -> do
      control.exLoad .= True
      let addr = rs1Data + imm            -- unaligned
          addr' = addr .&. complement 0x3 -- aligned
      if | op == Lb || op == Lbu -> meIR ?= MeLoad op rdAddr addr' (byteMask addr)
         | op == Lh || op == Lhu -> do
             meRvfi.rvfiTrap ||= isMisalignedHalf addr -- trap on half-word boundary
             meIR ?= MeLoad op rdAddr addr' (halfMask addr)
         | otherwise -> do -- Lw
             meRvfi.rvfiTrap ||= isMisaligned addr -- trap on word boundary
             meIR ?= MeLoad op rdAddr addr' 0xF
    ExAlu op rd -> do
      scribeAlu op rs1Data rs2Data
      meIR ?= MeRegWr rd
    ExAluImm op rd imm -> do
      scribeAlu op rs1Data imm
      meIR ?= MeRegWr rd
  where
    scribeAlu op in1 in2 = do
      scribe toAluOp     $ First $ Just op
      scribe toAluInput1 $ First $ Just in1
      scribe toAluInput2 $ First $ Just in2

    guardZero  -- register x0 always has value 0.
      :: MonadState s m 
      => Lens' s (Unsigned 5) 
      -> BitVector 32 
      -> m (BitVector 32)
    guardZero rsAddr rsValue = do
      isZero <- uses rsAddr (== 0)
      return $ if isZero
        then 0
        else rsValue
    regFwd 
      :: MonadState s m 
      => MonadReader r m
      => Lens' s (Unsigned 5) 
      -> Lens' r (BitVector 32)
      -> Lens' s (Maybe (Unsigned 5, BitVector 32))
      -> Lens' s (Maybe (Unsigned 5, BitVector 32))
      -> m (BitVector 32)
    regFwd rsAddr rsData meFwd wbFwd = 
      guardZero rsAddr =<< fwd <$> use rsAddr <*> view rsData <*> use meFwd <*> use wbFwd

-- | Decode stage
decode :: RWS ToPipe FromPipe Pipe ()
decode = do
  exIR .= Nothing
  exRvfi .= mkRvfi
  isFirstCycle  <- control.firstCycle <<.= False -- first memory output undefined
  isMeBranching <- uses (control.meBranching) isJust
  isWbMemory    <- use $ control.wbMemory
  isExLoad      <- use $ control.exLoad
  isExBranching <- use $ control.exBranching
  unless (isFirstCycle || isMeBranching || isWbMemory || isExLoad || isExBranching) $ do
    mem <- view fromMem
    pc <- use dePC
    case parseInstr mem of
      Right instr -> do
        exIR ?= instr
        exPC .= pc
        exRvfi.rvfiInsn .= mem
        control.deLoad .= case instr of
          ExLoad{} -> True
          _        -> False
        scribe toRs1Addr . First . Just =<< exRvfi.rvfiRs1Addr <<~ exRs1 <.= sliceRs1 mem
        scribe toRs2Addr . First . Just =<< exRvfi.rvfiRs2Addr <<~ exRs2 <.= sliceRs2 mem
      Left IllegalInstruction -> fetchPC .= pc -- roll-back PC, should handle trap
        
-- | fetch instruction
fetch :: RWS ToPipe FromPipe Pipe ()
fetch = do
  use (control.meBranching) >>= mapM_ (assign fetchPC)
  scribe toMem . First . Just . instrMem =<< use fetchPC
  isMeMemory <- use $ control.meMemory
  isDeLoad   <- use $ control.deLoad
  unless (isMeMemory || isDeLoad) $ dePC <~ fetchPC <<+= 4  

-------------
-- Utility --
-------------

-- | forward register writes
fwd 
  :: Unsigned 5 
  -> BitVector 32 
  -> Maybe (Unsigned 5, BitVector 32) -- ^ meRegFwd
  -> Maybe (Unsigned 5, BitVector 32) -- ^ wbRegFwd
  -> BitVector 32
fwd _    wr Nothing Nothing = wr
fwd addr wr Nothing (Just (wbAddr, wbWr))
  | addr == wbAddr = wbWr
  | otherwise      = wr
fwd addr wr (Just (meAddr, meWr)) Nothing
  | addr == meAddr = meWr
  | otherwise      = wr
fwd addr wr (Just (meAddr, meWr)) (Just (wbAddr, wbWr))
  | addr == meAddr = meWr
  | addr == wbAddr = wbWr
  | otherwise      = wr

-- | calcluate byte mask based on address
byteMask :: BitVector 32 -> BitVector 4
byteMask = (1 `shiftL`) . unpack . resize . slice d1 d0

-- | calculate half word mask based on address
halfMask :: BitVector 32 -> BitVector 4
halfMask addr = if addr .&. 0x2 == 0
                  then 0x3
                  else 0xC

-- | slice address based on mask
sliceByte :: BitVector 4 -> BitVector 32 -> BitVector 8
sliceByte = \case
  $(bitPattern "...1") -> slice d7  d0
  $(bitPattern "..1.") -> slice d15 d8
  $(bitPattern ".1..") -> slice d23 d16
  $(bitPattern "1...") -> slice d31 d24
  _ -> const 0

-- | slice address based on mask
sliceHalf :: BitVector 4 -> BitVector 32 -> BitVector 16
sliceHalf = \case
  $(bitPattern "..11") -> slice d15 d0
  $(bitPattern "11..") -> slice d31 d16
  _ -> const 0

-- | check if memory address misaligned on word boundary
isMisaligned :: (Bits a, Num a) => a -> Bool
isMisaligned a = a .&. 0x3 /= 0

-- | check if memory address misaligned on half-word boundary
isMisalignedHalf :: (Bits a, Num a) => a -> Bool
isMisalignedHalf a = a .&. 0x1 /= 0

-- | run monadic action when instruction is Just
withInstr :: MonadState s m => Lens' s (Maybe a) -> (a -> m ()) -> m ()
withInstr l k = use l >>= mapM_ k

-- | Hazards Note
--
-- Key:
-- J  = JAL
-- JR = JALR
-- O  = Bubble
-- S  = Store
-- *  = Stall
-- B  = Branch
-- 
-- Jump/Branch (Except JALR) -- OLD
-- +----+-----+-----+----+----+
-- | IF | DE  | EX  | ME | WB |
-- +====+=====+=====+====+====+
-- | 4  | --- | --- | -- | -- |   
-- +----+-----+-----+----+----+
-- | 8  | J15 | --- | -- | -- |
-- +----+-----+-----+----+----+
-- | 15 |  O  | J15 | -- | -- |
-- +----+-----+-----+----+----+
--
-- Jump/Branch
-- +----+------+------+------+----+
-- | IF |  DE  |  EX  |  ME  | WB |
-- +====+======+======+======+====+
-- | 4  | ---- | ---- | ---- | -- |
-- +----+------+------+------+----+
-- | 8  | JR20 | ---- | ---- | -- |
-- +----+------+------+------+----+
-- | 12 |  O   | JR20 | ---- | -- |
-- +----+------+------+------+----+
-- | 20 |  O   |  O   | JR20 | -- |
-- +----+------+------+------+----+
--
-- Store
-- +-------+------+------+------+----+
-- |  IF   | DE   |  EX  |  ME  | WB |
-- +=======+======+======+======+====+
-- | 4     | ---- | ---- | ---- | -- |
-- +-------+------+------+------+----+
-- | 8     |  S   | ---- | ---- | -- |
-- +-------+------+------+------+----+
-- | 12    | J100 |  S   | ---- | -- |
-- +-------+------+------+------+----+
-- | *100* |  O   | J100 |  S   | -- |
-- +-------+------+------+------+----+
-- |  100  |  O   |  O   | J100 | S  |
-- +-------+------+------+------+----+
--
-- Load
-- +------+------+------+----+----+
-- | IF   |  DE  |  EX  | ME | WB |
-- +======+======+======+====+====+
-- | 4    | ---- | ---- | -- | -- |
-- +------+------+------+----+----+
-- | *8*  |  L   | ---- | -- | -- |
-- +------+------+------+----+----+
-- | 8    |  O   |  L   | -- | -- |
-- +------+------+------+----+----+
-- | *12* | B100 |  O   | L  | -- |
-- +------+------+------+----+----+
-- | 100  |  O   | B100 | O  | L  |
-- +------+------+------+----+----+
