module Bus where

import Clash.Prelude
import Lion.Core (ToMem(..))

-- | SoC Memory/Peripheral access bus
data Bus = Bus
  { busAddr   :: BitVector 32
  , busMask   :: BitVector 4
  , busStoreM :: Maybe (BitVector 32)
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX  

-- | Construct Bus by forgetting whether memory access is instruction or memory.
mkBus :: ToMem -> Bus
mkBus = \case
  InstrMem addr        -> Bus addr 0xF Nothing
  DataMem addr msk wrM -> Bus addr msk wrM

