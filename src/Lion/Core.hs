{-|
Module      : Lion.Core
Description : Lion RISC-V Core
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com

The Lion RISC-V Core is a 32-bit processor written in Haskell using [Clash](https://clash-lang.org). Note, all peripherals and memory must have single cycle latency. See [lion-soc](https://github.com/standardsemiconductor/lion/tree/main/lion-soc) for an example of using the Lion core in a system.
-}

module Lion.Core 
  ( core
  , FromCore(..)
  , toMem
  , toRvfi
  , P.ToMem(..)
  ) where

import Clash.Prelude
import Control.Lens
import Data.Maybe
import Data.Monoid
import Lion.Rvfi
import qualified Lion.Pipe as P

-- | Core outputs
data FromCore dom = FromCore
  { _toMem  :: Signal dom (Maybe P.ToMem)
  , _toRvfi :: Signal dom Rvfi
  }
makeLenses ''FromCore

-- | RISC-V Core
core
  :: HiddenClockResetEnable dom
  => BitVector 32               -- ^ start address
  -> Signal dom (BitVector 32)  -- ^ core input, from memory/peripherals
  -> FromCore dom               -- ^ core output
core start toCore = FromCore
  { _toMem  = getFirst . P._toMem <$> fromPipe
  , _toRvfi = fromMaybe mkRvfi . getFirst . P._toRvfi <$> fromPipe
  }
  where
    fromPipe = P.pipe start $ P.ToPipe <$> rs1Data <*> rs2Data <*> toCore
    rs1Addr = fromMaybe 0 . getFirst . P._toRs1Addr <$> fromPipe
    rs2Addr = fromMaybe 0 . getFirst . P._toRs2Addr <$> fromPipe
    rdWrM = getFirst . P._toRd <$> fromPipe
    (rs1Data, rs2Data) = regBank rs1Addr rs2Addr rdWrM

-- | Register bank
regBank
  :: HiddenClockResetEnable dom
  => Signal dom (Unsigned 5)                        -- ^ Rs1 Addr
  -> Signal dom (Unsigned 5)                        -- ^ Rs2 Addr
  -> Signal dom (Maybe (Unsigned 5, BitVector 32))  -- ^ Rd Write
  -> Unbundled dom (BitVector 32, BitVector 32)     -- ^ (Rs1Data, Rs2Data)
regBank rs1Addr rs2Addr rdWrM = (regFile rs1Addr, regFile rs2Addr)
  where
    regFile = flip (readNew (blockRamPow2 (repeat 0))) rdWrM