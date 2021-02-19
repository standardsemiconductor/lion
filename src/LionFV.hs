module LionFV where

import Clash.Prelude
import Clash.Annotations.TH
import Core                 ( core, FromCore(FromCore), ToCore(ToCore) )
import Data.Maybe           ( fromMaybe, isJust )
import Rvfi                 ( Rvfi )
import Ice40.Clock          ( Lattice12Mhz )

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
  , rvfi
  )
  where
    FromCore toMem rvfi = core $ ToCore $ Just <$> memRData
    memValid = isJust <$> toMem
    memInstr = pure True -- TODO FIX!
    memAddr  = maybe 0 fst <$> toMem
    memWData = fromMaybe 0 . (snd =<<) <$> toMem
    memWStrb = pure 0 -- TODO FIX!

{-# NOINLINE topEntity #-}
topEntity
  :: "clock" ::: Clock Lattice12Mhz
  -> "reset" ::: Reset Lattice12Mhz
  -> "mem_rdata" ::: Signal Lattice12Mhz (BitVector 32)
  -> ( "mem_valid" ::: Signal Lattice12Mhz Bool
     , "mem_instr" ::: Signal Lattice12Mhz Bool
     , "mem_addr"  ::: Signal Lattice12Mhz (BitVector 32)
     , "mem_wdata" ::: Signal Lattice12Mhz (BitVector 32)
     , "mem_wstrb" ::: Signal Lattice12Mhz (BitVector 4)
     , "rvfi" ::: Signal Lattice12Mhz Rvfi
     )
topEntity clk rst = exposeClockResetEnable lionFV clk rst enableGen
makeTopEntityWithName 'topEntity "LionFV"