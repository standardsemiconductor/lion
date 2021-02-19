module Core where

import Clash.Prelude

data ToCore dom = ToCore
  { _fromMem :: Signal dom (Maybe (BitVector 32))
  }
makeLenses ''ToCore

data FromCore dom = FromCore
  { _toMem  :: Signal dom (Maybe (BitVector 32, Maybe (BitVector 32)))
  , _toRvfi :: Signal dom Rvfi
  }
makeLenses ''FromCore

core
  :: HiddenClockResetEnable dom
  => ToCore dom
  -> FromCore dom
core fromMemory = toMemory
  where
    fromPipe = pipe $ ToPipe <$> rs1Data <*> rs2Data
    (rs1Data, rs2Data) = regBank rs1Addr rs2Addr rdWrM

regBank
  :: HiddenClockResetEnable dom
  => Signal dom (Unsigned 5)                        -- ^ Rs1 Addr
  -> Signal dom (Unsigned 5)                        -- ^ Rs2 Addr
  -> Signal dom (Maybe (Unsigned 5, BitVector 32))  -- ^ Rd Write
  -> Unbundled dom (BitVector 32, BitVector 32)     -- ^ (Rs1Data, Rs2Data)
regBank rs1Addr rs2Addr rdWrM = (regFile rs1Addr, regFile rs2Addr)
  where
    regFile = flip (readNew (blockRamPow2 (repeat 0))) rdWrM