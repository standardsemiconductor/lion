{-|
Module      : Lion.Pipe.Monad
Description : Pipeline Monad
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com
-}

module Lion.Pipe.Monad where

import Clash.Prelude
import Control.Lens
import Control.Monad.RWS

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
  , _exBranching :: Maybe (BitVector 32)             -- ^ execute stage branch/jump
  , _meBranching :: Bool                             -- ^ memory stage branch/jump
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
  , _exBranching = Nothing
  , _meBranching = False
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

--------------------
-- Pipeline Monad --
--------------------
newtype PipeM c = PipeM{ unPipe :: RWS (ToPipe c) (FromPipe c) (Pipe c) }
  deriving ( Functor, Applicative, Monad
           , MonadRWS (ToPipe c) (FromPipe c) (Pipe c)
           )