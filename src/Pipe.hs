module Pipe where

import Clash.Prelude
import Control.Lens
import Control.Monad.RWS

data ToPipe = ToPipe
  { _fromRs1 :: BitVector 32
  , _fromRs2 :: BitVector 32
  , _fromMem :: Maybe (BitVector 32)
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''ToPipe

data FromPipe = FromPipe
  { _toMemAddr :: First (BitVector 32)
  , _toMemData :: First (BitVector 32)
  , _toRs1Addr :: First (BitVector 5)
  , _toRs2Addr :: First (BitVector 5)
  , _toRd      :: First (BitVector 5, BitVector 32)
  , _toRvfi    :: Rvfi
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

  -- execute stage
  , _exIR     :: Instr
  , _exRvfi   :: Rvfi

  -- memory stage
  , _meIR     :: Instr
  , _meAluOut :: BitVector 32
  , _meRvfi   :: Rvfi

  -- writeback stage
  , _wbIR     :: Instr
  , _wbAluOut :: BitVector 32
  , _wbNRet   :: BitVector 64
  , _wbFlags  :: Flags
  , _wbRvfi   :: Rvfi
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''Pipe

data Flags = Flags
  { _trap :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''Flags

pipeM :: RWS ToPipe FromPipe Pipe ()
pipeM = do
  writeback
  memory
  execute
  decode
  fetch

writeback :: RWS ToPipe FromPipe Pipe ()
writeback = do
  isTrap <- use $ wbFlags.trap
  unless isTrap $ wbNRet += 1
  instr <- use wbIR
  scribe toRd . First . Just =<< (rd instr,) <$> wbRvfi.rvfiRdWData <<~ use wbAluOut

memory :: RWS ToPipe FromPipe Pipe ()
memory = do
  _ <- wbIR <<~ use meIR
  wbAluOut <~ use meAluOut

execute :: RWS ToPipe FromPipe Pipe ()
execute = do
  _ <- memIR <<~ use exIR
  rs1Data <- use fromRs1
  rs2Data <- use fromRs2
  meAluOut .= rs1Data + rs2Data

decode :: RWS ToPipe FromPipe Pipe ()
decode = do
  memM <- view fromMem
  forM_ memM $ \mem -> do
    instr <- exIR <.= mem
    scribe toRs1Addr . First . Just =<< exRvfi.rvfiRs1Addr <.= rs1 instr
    scribe toRs2Addr . First . Just =<< exRvfi.rvfiRs2Addr <.= rs2 instr
  
fetch :: RWS ToPipe FromPipe Pipe ()
fetch = do
  pc <- dePC <<~ use fetchPC
  fetchPC <~ deNPC <.= pc + 4