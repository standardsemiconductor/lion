{-|
Module      : LionFV
Description : Lion Formal Verification
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com
-}

module LionFV where

import Clash.Prelude
import Clash.Annotations.TH
import Data.Maybe           ( fromMaybe, isJust )
import Ice40.Clock          ( Lattice12Mhz )
import Lion.Core            (core, defaultCoreConfig, FromCore(..), ToMem(..) )
import Lion.Rvfi            ( Rvfi )

lionFV
  :: HiddenClockResetEnable dom
  => Signal dom (BitVector 32)   -- ^ mem_rdata
  -> ( Signal dom Bool           -- mem_valid
     , Signal dom Bool           -- mem_instr
     , Signal dom (BitVector 32) -- mem_addr
     , Signal dom (BitVector 32) -- mem_wdata
     , Signal dom (BitVector 4)  -- mem_wstrb
     , Signal dom Rvfi           -- rvfi
     )
lionFV memRData = 
  ( memValid
  , memInstr
  , memAddr
  , memWData
  , memWStrb
  , toRvfi fromCore
  )
  where
    fromCore = core defaultCoreConfig memRData
    memValid = isJust <$> toMem fromCore
    memInstr = fromMaybe False . fmap isInstr <$> toMem fromCore
    memAddr  = maybe 0 getAddr <$> toMem fromCore
    memWData = fromMaybe 0 . (getData =<<) <$> toMem fromCore
    memWStrb = fromMaybe 0 . fmap getMask <$> toMem fromCore

isInstr :: ToMem -> Bool
isInstr = \case
  InstrMem _    -> True
  DataMem _ _ _ -> False

getAddr :: ToMem -> BitVector 32
getAddr = \case
  InstrMem a     -> a
  DataMem  a _ _ -> a

getData :: ToMem -> Maybe (BitVector 32)
getData = \case
  InstrMem _     -> Nothing
  DataMem  _ _ d -> d

getMask :: ToMem -> BitVector 4
getMask = \case
  InstrMem _     -> 0xF
  DataMem  _ m _ -> m

{-# NOINLINE topEntity #-}
topEntity
  :: "clock"     ::: Clock Lattice12Mhz
  -> "reset"     ::: Reset Lattice12Mhz
  -> "mem_rdata" ::: Signal Lattice12Mhz (BitVector 32)
  -> ( "mem_valid" ::: Signal Lattice12Mhz Bool
     , "mem_instr" ::: Signal Lattice12Mhz Bool
     , "mem_addr"  ::: Signal Lattice12Mhz (BitVector 32)
     , "mem_wdata" ::: Signal Lattice12Mhz (BitVector 32)
     , "mem_wstrb" ::: Signal Lattice12Mhz (BitVector 4)
     , "rvfi"      ::: Signal Lattice12Mhz Rvfi
     )
topEntity clk rst = exposeClockResetEnable lionFV clk rst enableGen
makeTopEntityWithName 'topEntity "LionFV"