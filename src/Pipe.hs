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

data Flags = Flags
  { _branching :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''Flags

mkFlags :: Flags
mkFlags = Flags
  { _branching = False
  }

data Pipe = Pipe
  { _fetchPC  :: BitVector 32

  -- decode stage
  , _dePC     :: BitVector 32
  , _deNPC    :: BitVector 32

  -- execute stage
  , _exIR      :: Maybe ExInstr
  , _exPC      :: BitVector 32
  , _exRs1Zero :: Bool
  , _exRs2Zero :: Bool
  , _exRvfi    :: Rvfi

  -- memory stage
  , _meIR     :: Maybe MeInstr
  , _meRvfi   :: Rvfi

  -- writeback stage
  , _wbIR     :: Maybe WbInstr
  , _wbNRet   :: BitVector 64
  , _wbRvfi   :: Rvfi

  -- pipeline flags
  , _flags :: Flags
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
  , _exIR      = Nothing
  , _exPC      = 0
  , _exRs1Zero = False
  , _exRs2Zero = False
  , _exRvfi    = mkRvfi

  -- memory stage
  , _meIR     = Nothing
  , _meRvfi   = mkRvfi
 
  -- writeback stage
  , _wbIR   = Nothing
  , _wbNRet = 0
  , _wbRvfi = mkRvfi
  
  -- pipeline flags
  , _flags = mkFlags
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
  flags .= mkFlags -- reset flags
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
        wbRvfi.rvfiRdAddr .= rdAddr
        rdData <- wbRvfi.rvfiRdWData <.= guardZero rdAddr wr
        scribe toRd $ First $ Just (rdAddr, rdData)
      WbNop -> return ()
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
        MeNop         -> wbIR ?= WbNop

execute :: RWS ToPipe FromPipe Pipe ()
execute = do
  meRvfi <~ use exRvfi
  use exIR >>= \instrM -> do
    meIR .= Nothing
    forM_ instrM $ \instr -> do
      pc <- use exPC
      rs1Data <- meRvfi.rvfiRs1Data <<~ guardZero exRs1Zero fromRs1
      rs2Data <- meRvfi.rvfiRs2Data <<~ guardZero exRs2Zero fromRs2
      meIR <~ case instr of
        Ex op rd imm -> case op of
          Lui   -> return $ Just $ MeRegWr rd imm
          Auipc -> return $ Just $ MeRegWr rd $ alu Add pc imm
          Jal   -> do
            flags.branching .= True
            npc <- fetchPC <<~ meRvfi.rvfiPcWData <.= pc + imm
            when (npc .&. 0x3 /= 0) $ meRvfi.rvfiTrap .= True
            return $ Just $ MeRegWr rd $ pc + 4
          Jalr  -> do
            flags.branching .= True
            npc <- fetchPC <<~ meRvfi.rvfiPcWData <.= clearBit (rs1Data + imm) 0
            when (npc .&. 0x3 /= 0) $ meRvfi.rvfiTrap .= True
            return $ Just $ MeRegWr rd $ pc + 4
        ExBranch op imm -> do -- TODO NEEDS WORK!
          when (branch op rs1Data rs2Data) $ do
            flags.branching .= True
            npc <- fetchPC <<~ meRvfi.rvfiPcWData <.= signedOffset pc imm
            when (npc .&. 0x3 /= 0) $ meRvfi.rvfiTrap .= True
          return $ Just MeNop
        ExAlu    op rd     -> return $ Just $ MeRegWr rd $ alu op rs1Data rs2Data
        ExAluImm op rd imm -> return $ Just $ MeRegWr rd $ alu op rs1Data imm
  where
    sign :: BitVector 32 -> Signed 32
    sign = unpack
    signedOffset pc imm
      | sign imm >= 0 = pc + imm
      | otherwise = pc - imm
    guardZero rsZero rsValue = do
      isZero <- use rsZero
      if isZero
        then return 0
        else view rsValue

decode :: RWS ToPipe FromPipe Pipe ()
decode = do
  exRvfi .= mkRvfi
  memM <- view fromMem
  isBranch <- use $ flags.branching
  forM_ memM $ \mem -> 
    case parseInstr mem of
      Right instr -> do
        exIR .= if isBranch 
          then Nothing
          else Just instr
        pc <- exPC <<~ exRvfi.rvfiPcRData <<~ use dePC
        exRvfi.rvfiPcWData .= pc + 4
        exRvfi.rvfiInsn .= mem
        scribe toRs1Addr . First . Just =<< exRvfi.rvfiRs1Addr <.= sliceRs1 mem
        scribe toRs2Addr . First . Just =<< exRvfi.rvfiRs2Addr <.= sliceRs2 mem
      Left _  -> do
        exRvfi.rvfiTrap .= True
        exIR .= Nothing
  
fetch :: RWS ToPipe FromPipe Pipe ()
fetch = do
  pc <- dePC <<~ use fetchPC
  scribe toMem $ First $ Just (pc, Nothing)
  fetchPC += 4

