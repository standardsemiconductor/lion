module Soc where

import Clash.Prelude
import Core (FromCore(FromCore), ToCore(ToCore), core)
import Ice40.Clock
import Ice40.Osc
import Ice40.Rgb
import Ice40.Led

---------
-- RGB --
---------
type Rgb = ("red" ::: Bit, "green" ::: Bit, "blue" ::: Bit)



rgb :: Signal dom ToMem -> Signal dom Rgb
rgb mem = rgbPrim "0b0" "0b111111" "0b111111" "0b111111" (pure 1) (pure 1) r g b
  where
    (r, g, b, _) = led cs wr addr en exe
----------
-- BIOS --
----------
bios
  :: HiddenClockResetEnable dom
  => Signal dom ToMem
  -> Signal dom (BitVector 32)
bios _ = concat4 <$> b3 <*> b2 <*> b1 <*> b0
  where
    b3 = romFilePow2 "_build/bios/Bios.rom3" addr
    b2 = romFilePow2 "_build/bios/Bios.rom2" addr
    b1 = romFilePow2 "_build/bios/Bios.rom1" addr
    b0 = romFilePow2 "_build/bios/Bios.rom0" addr

concat4
  :: KnownNat n
  => BitVector n
  -> BitVector n
  -> BitVector n
  -> BitVector n
  -> BitVector (4*n)
concat4 b3 b2 b1 b0 = b3 ++# b2 ++# b1 ++# b0

--------------
-- Lion SOC --
--------------
lion :: HiddenClockResetEnable dom => Signal dom Rgb
lion = rgb toMem
  where
    FromCore toMem _ = core $ ToCore fromBios
    fromBios = fromCore^.toMem.to bios
    
----------------
-- Top Entity --
----------------
{-# NOINLINE topEntity #-}
topEntity :: "led" ::: Signal Lattice12Mhz Rgb
topEntity = withClockResetEnable clk latticeRst enableGen lion
  where
    clk = hf12Mhz (pure True :: Signal Lattice12Mhz Bool)
                  (pure True :: Signal Lattice12Mhz Bool)
makeTopEntityWithName 'topEntity "Soc"