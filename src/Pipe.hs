module Pipe where

import Clash.Prelude
import Control.Lens hiding ( op )
import Control.Monad.RWS
import Data.Monoid.Generic
import Instruction
import Rvfi

data ToPipe = ToPipe
  { _fromRs1 :: BitVector 32
  , _fromRs2 :: BitVector 32
  , _fromMem :: Maybe (BitVector 32)
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''ToPipe

data FromPipe = FromPipe
  { _toMem     :: First (BitVector 32, Maybe (BitVector 32))
  , _toRs1Addr :: First (Unsigned 5)
  , _toRs2Addr :: First (Unsigned 5)
  , _toRd      :: First (Unsigned 5, BitVector 32)
  , _toRvfi    :: First Rvfi
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
  deriving Semigroup via GenericSemigroup FromPipe
  deriving Monoid via GenericMonoid FromPipe
makeLenses ''FromPipe

data Pipe = Pipe
  { _fetchPC  :: BitVector 32

  -- decode stage
  , _dePC     :: BitVector 32
  , _deNPC    :: BitVector 32

  -- execute stage
  , _exIR     :: Maybe ExInstr
  , _exPC     :: BitVector 32
  , _exRvfi   :: Rvfi

  -- memory stage
  , _meIR     :: Maybe MeInstr
  , _meRvfi   :: Rvfi

  -- writeback stage
  , _wbIR     :: Maybe WbInstr
  , _wbNRet   :: BitVector 64
  , _wbRvfi   :: Rvfi
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''Pipe

mkPipe :: Pipe
mkPipe = Pipe
  { _fetchPC  = 0  

  -- decode stage 
  , _dePC     = 0
  , _deNPC    = 0
  
  -- execute stage
  , _exIR     = Nothing
  , _exPC     = 0
  , _exRvfi   = mkRvfi

  -- memory stage
  , _meIR     = Nothing
  , _meRvfi   = mkRvfi
 
  -- writeback stage
  , _wbIR   = Nothing
  , _wbNRet = 0
  , _wbRvfi = mkRvfi
  }

pipe 
  :: HiddenClockResetEnable dom
  => Signal dom ToPipe
  -> Signal dom FromPipe
pipe = mealy pipeMealy mkPipe
  where
    pipeMealy s i = let ((), s', o) = runRWS pipeM i s
                    in (s', o) 

pipeM :: RWS ToPipe FromPipe Pipe ()
pipeM = do
  writeback
  memory
  execute
  decode
  fetch

writeback :: RWS ToPipe FromPipe Pipe ()
writeback = use wbIR >>= \instrM ->
  forM_ instrM $ \instr -> do
    wbRvfi.rvfiValid .= True
    wbRvfi.rvfiOrder <~ wbNRet <<+= 1
    case instr of
      WbRegWr rdAddr wr -> do
        rdData <- wbRvfi.rvfiRdWData <.= guardZero rdAddr wr
        scribe toRd $ First $ Just (rdAddr, rdData)
    scribe toRvfi . First . Just =<< use wbRvfi
  where
    guardZero 0 = const 0
    guardZero _ = id

memory :: RWS ToPipe FromPipe Pipe ()
memory = do
  wbRvfi <~ use meRvfi
  use meIR >>= \instrM -> do
    wbIR .= Nothing
    forM_ instrM $ \instr -> do
      case instr of
        MeRegWr rd wr -> wbIR ?= WbRegWr rd wr

execute :: RWS ToPipe FromPipe Pipe ()
execute = do
  meRvfi <~ use exRvfi
  use exIR >>= \instrM -> do
    meIR .= Nothing
    forM_ instrM $ \instr -> do
      pc <- use exPC
      rs1Data <- meRvfi.rvfiRs1Data <<~ view fromRs1
      rs2Data <- meRvfi.rvfiRs2Data <<~ view fromRs2
      meIR <~ case instr of
        Ex op rd imm -> case op of
          Lui   -> return $ Just $ MeRegWr rd imm
          Auipc -> return $ Just $ MeRegWr rd $ alu Add pc imm
          Jal   -> do
            npc <- fetchPC <<~ meRvfi.rvfiPcWData <.= pc + imm
            when (npc .&. 0x3 /= 0) $ meRvfi.rvfiTrap .= True
            return $ Just $ MeRegWr rd $ pc + 4
          Jalr  -> do
            npc <- fetchPC <<~ meRvfi.rvfiPcWData <.= (rs1Data + imm) .&. 0xFFFFFFFE
            when (npc .&. 0x3 /= 0) $ meRvfi.rvfiTrap .= True
            return $ Just $ MeRegWr rd $ pc + 4
        ExAlu    op rd     -> return $ Just $ MeRegWr rd $ alu op rs1Data rs2Data
        ExAluImm op rd imm -> return $ Just $ MeRegWr rd $ alu op rs1Data imm

decode :: RWS ToPipe FromPipe Pipe ()
decode = do
  exRvfi .= mkRvfi
  memM <- view fromMem
  forM_ memM $ \mem -> 
    case parseInstr mem of
      Right instr -> do
        exIR .= Just instr
        pc <- exPC <<~ exRvfi.rvfiPcRData <<~ use dePC
        exRvfi.rvfiPcWData .= pc + 4
        exRvfi.rvfiInsn .= mem
        scribe toRs1Addr . First . Just =<< exRvfi.rvfiRs1Addr <.= sliceRs1 mem
        scribe toRs2Addr . First . Just =<< exRvfi.rvfiRs2Addr <.= sliceRs2 mem
        exRvfi.rvfiRdAddr .= sliceRd mem
      Left _  -> do
        exRvfi.rvfiTrap .= True
        exIR .= Nothing
  
fetch :: RWS ToPipe FromPipe Pipe ()
fetch = do
  pc <- dePC <<~ use fetchPC
  scribe toMem $ First $ Just (pc, Nothing)
  fetchPC <~ deNPC <.= pc + 4

