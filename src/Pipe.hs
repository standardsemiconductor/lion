module Pipe where

import Clash.Prelude
import Control.Monad.RWS

data ToPipe = ToPipe
  { _fromRs1 :: BitVector 32
  , _fromRs2 :: BitVector 32
  , _fromMem :: BitVector 32
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''ToPipe

data FromPipe = FromPipe
  { _toMemAddr :: First (BitVector 32)
  , _toMemData :: First (BitVector 32)
  , _toRs1Addr :: First (BitVector 5)
  , _toRs2Addr :: First (BitVector 5)
  , _toRdData  :: First (BitVector 32)
  , _toRvfi    :: Rvfi
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
  deriving Semigroup via GenericSemigroup FromPipe
  deriving Monoid via GenericMonoid FromPipe
makeLenses ''FromPipe

data Pipe = Pipe
  { _pc :: BitVector 32
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''Pipe

pipeM :: MonadRWS r w s m => m ()
pipeM = _