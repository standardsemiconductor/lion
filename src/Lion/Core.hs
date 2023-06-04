{-|
Module      : Lion.Core
Description : Lion RISC-V Core
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com

The Lion core is a 32-bit [RISC-V](https://riscv.org/about/) processor written in Haskell using [Clash](https://clash-lang.org). Note, all peripherals and memory must have single cycle latency. See [lion-soc](https://github.com/standardsemiconductor/lion/tree/main/lion-soc) for an example of using the Lion core in a system.
-}

module Lion.Core 
  ( core
  , defaultCoreConfig
  , P.defaultPipeConfig
  , CoreConfig(..)
  , AluConfig(..)
  , P.PipeConfig(..)
  , FromCore(..)
  , P.ToMem(..)
  , P.MemoryAccess(..)
  ) where

import Clash.Prelude
import Data.Maybe
import Data.Monoid
import Lion.Alu
import Lion.Rvfi
import qualified Lion.Pipe as P
import qualified Lion.Instruction as I (Op(Add))

-- | Core configuration
data CoreConfig = CoreConfig
  { aluConfig  :: AluConfig    -- ^ ALU configuration, default: `Soft`
  , pipeConfig :: P.PipeConfig -- ^ pipeline configuration
  }
  deriving stock (Generic, Show, Eq)

-- | Default core configuration
--
-- `aluConfig` = `Soft`
--
-- `pipeConfig` = `P.defaultPipeConfig`
defaultCoreConfig :: CoreConfig
defaultCoreConfig = CoreConfig
  { aluConfig  = Soft
  , pipeConfig = P.defaultPipeConfig
  }

-- | Core outputs
data FromCore dom = FromCore
  { toMem  :: Signal dom (Maybe P.ToMem) -- ^ shared memory and instruction bus, output from core to memory and peripherals
  , toRvfi :: Signal dom Rvfi -- ^ formal verification interface output, see [lion-formal](https://github.com/standardsemiconductor/lion/tree/main/lion-formal) for usage
  }

-- | RISC-V Core: RV32I
core
  :: HiddenClockResetEnable dom
  => CoreConfig                 -- ^ core configuration
  -> Signal dom (BitVector 32)  -- ^ core input, from memory/peripherals
  -> FromCore dom               -- ^ core output
core config toCore = FromCore
  { toMem  = getFirst . P._toMem <$> fromPipe
  , toRvfi = fromMaybe mkRvfi . getFirst . P._toRvfi <$> fromPipe
  }
  where
    -- alu connection
    aluOp = fromMaybe I.Add . getFirst . P._toAluOp     <$> fromPipe
    aluInput1 = fromMaybe 0 . getFirst . P._toAluInput1 <$> fromPipe
    aluInput2 = fromMaybe 0 . getFirst . P._toAluInput2 <$> fromPipe
    aluOutput = alu (aluConfig config) aluOp aluInput1 aluInput2

    -- reg bank connection
    rs1Addr = fromMaybe 0 . getFirst . P._toRs1Addr <$> fromPipe
    rs2Addr = fromMaybe 0 . getFirst . P._toRs2Addr <$> fromPipe
    rdWrM = getFirst . P._toRd <$> fromPipe
    (rs1Data, rs2Data) = regBank rs1Addr rs2Addr rdWrM

    -- pipeline connection
    fromPipe = P.pipe (pipeConfig config) $ P.ToPipe <$> rs1Data 
                                                     <*> rs2Data 
                                                     <*> aluOutput
                                                     <*> toCore

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
