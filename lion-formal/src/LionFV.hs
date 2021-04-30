{-|
Module      : LionFV
Description : Lion Formal Verification
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module LionFV where

import Clash.Prelude
import Clash.Annotations.TH
import Data.Maybe           ( fromMaybe, isJust )
import Lion.Core            (core, defaultCoreConfig, FromCore(..)
                            , ToMem(..), MemoryAccess(InstrMem) )
import Lion.Rvfi            ( Rvfi )

lionFV
  :: HiddenClockResetEnable dom
  => (KnownNat xl, KnownNat (Log2 xl), 1 <= Div xl 8)
  => Signal dom (BitVector xl)   -- ^ mem_rdata
  -> ( Signal dom Bool           -- mem_valid
     , Signal dom Bool           -- mem_instr
     , Signal dom (BitVector xl) -- mem_addr
     , Signal dom (BitVector xl) -- mem_wdata
     , Signal dom (BitVector (Div xl 8)) -- mem_wstrb
     , Signal dom (Rvfi xl)      -- rvfi
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
    memInstr = maybe False isInstr <$> toMem fromCore
    memAddr  = maybe 0 memAddress <$> toMem fromCore
    memWData = fromMaybe 0 . (memWrite =<<) <$> toMem fromCore
    memWStrb = maybe 0 memByteMask <$> toMem fromCore

isInstr :: ToMem xl -> Bool
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
     , "rvfi"      ::: Signal System (Rvfi 32)
     )
topEntity clk rst = exposeClockResetEnable lionFV clk rst enableGen
makeTopEntityWithName 'topEntity "LionFV"