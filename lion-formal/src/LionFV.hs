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
import Lion.Core            (core, defaultCoreConfig, FromCore(..)
                            , ToMem(..), MemoryAccess(InstrMem) )
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
    memAddr  = maybe 0 memAddress <$> toMem fromCore
    memWData = fromMaybe 0 . (memWrite =<<) <$> toMem fromCore
    memWStrb = fromMaybe 0 . fmap memByteMask <$> toMem fromCore

isInstr :: ToMem -> Bool
isInstr = (== InstrMem) . memAccess

{-# NOINLINE topEntity #-}
topEntity
  :: "clock"     ::: Clock System
  -> "reset"     ::: Reset System
  -> "mem_rdata" ::: Signal System (BitVector 32)
  -> ( "mem_valid" ::: Signal System Bool
     , "mem_instr" ::: Signal System Bool
     , "mem_addr"  ::: Signal System (BitVector 32)
     , "mem_wdata" ::: Signal System (BitVector 32)
     , "mem_wstrb" ::: Signal System (BitVector 4)
     , "rvfi"      ::: Signal System Rvfi
     )
topEntity clk rst = exposeClockResetEnable lionFV clk rst enableGen
makeTopEntityWithName 'topEntity "LionFV"