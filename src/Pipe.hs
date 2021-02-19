module Pipe where

import Clash.Prelude
import Control.Lens
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
  , _exIR     :: Maybe Alu
  , _exPC     :: BitVector 32
  , _exRvfi   :: Rvfi

  -- memory stage
  , _meIR     :: Maybe Alu
  , _meAluOut :: BitVector 32
  , _meRvfi   :: Rvfi

  -- writeback stage
  , _wbIR     :: Maybe Alu
  , _wbAluOut :: BitVector 32
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
  , _meAluOut = 0
  , _meRvfi   = mkRvfi
 
  -- writeback stage
  , _wbIR     = Nothing
  , _wbAluOut = 0
  , _wbNRet   = 0
  , _wbRvfi   = mkRvfi
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
    rdData <- wbRvfi.rvfiRdWData <<~ use wbAluOut
    case instr of
      Op    _ rd   -> scribe toRd $ First $ Just (rd, rdData)
      Opimm _ rd _ -> scribe toRd $ First $ Just (rd, rdData)
    scribe toRvfi . First . Just =<< use wbRvfi

memory :: RWS ToPipe FromPipe Pipe ()
memory = do
  wbRvfi <~ use meRvfi
  _ <- wbIR <<~ use meIR
  wbAluOut <~ use meAluOut

execute :: RWS ToPipe FromPipe Pipe ()
execute = do
  meRvfi <~ use exRvfi
  instrM <- meIR <<~ use exIR
  forM_ instrM $ \instr -> do
    meRvfi.rvfiPcRData <~ use exPC
    meRvfi.rvfiPcWData <~ uses exPC (+ 4)
    rs1Data <- meRvfi.rvfiRs1Data <<~ view fromRs1
    rs2Data <- meRvfi.rvfiRs2Data <<~ view fromRs2
    assign meAluOut $ alu (getOp instr) rs1Data $ case instr of
      Op    _ _   -> rs2Data
      Opimm _ _ i -> i

decode :: RWS ToPipe FromPipe Pipe ()
decode = do
  exRvfi .= mkRvfi
  memM <- view fromMem
  forM_ memM $ \mem -> 
    case parseAlu mem of
      Right instr -> do
        exIR .= Just instr
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
  fetchPC <~ deNPC <.= pc + 4
