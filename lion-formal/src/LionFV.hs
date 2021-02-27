module LionFV where

import Clash.Prelude
import Clash.Annotations.TH
import Data.Maybe           ( fromMaybe, isJust )
import Ice40.Clock          ( Lattice12Mhz )
import Lion.Core            (core, FromCore(FromCore), ToCore(ToCore), ToMem(..) )
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
  , rvfi
  )
  where
    FromCore toMem rvfi = core $ ToCore memRData
    memValid = isJust <$> toMem
    memInstr = fromMaybe False . fmap isInstr <$> toMem
    memAddr  = maybe 0 getAddr <$> toMem
    memWData = fromMaybe 0 . (getData =<<) <$> toMem
    memWStrb = fromMaybe 0 . fmap getMask <$> toMem

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