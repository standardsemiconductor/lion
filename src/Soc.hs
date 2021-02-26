module Soc where

import Clash.Prelude
import Clash.Annotations.TH
import Core (FromCore(FromCore), ToCore(ToCore), ToMem(DataMem, InstrMem), core)
import Data.Functor ( (<&>) )
import Data.Maybe ( fromMaybe )
import Ice40.Clock
import Ice40.Osc
import Ice40.Rgb
import Ice40.Led

---------
-- RGB --
---------
type Rgb = ("red" ::: Bit, "green" ::: Bit, "blue" ::: Bit)

rgb :: HiddenClock dom => Signal dom (Maybe ToMem) -> Signal dom Rgb
rgb mem = rgbPrim "0b0" "0b111111" "0b111111" "0b111111" (pure 1) (pure 1) r g b
  where
    (wr, addr, en) = unbundle $ mem <&> \case
      Just (DataMem 
              a@($(bitPattern "0000000000000000000000010000....")) 
              $(bitPattern "0001")
              (Just d)
           ) -> (slice d7 d0 d, slice d3 d0 a, True)
      _ -> (0, 0, False)
    (r, g, b, _) = led (pure 1) wr addr en (pure True)

----------
-- BIOS --
----------
bios
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe ToMem)
  -> Signal dom (BitVector 32)
bios mem = concat4 <$> b3 <*> b2 <*> b1 <*> b0
  where
    addr = unpack . slice d8 d0 . fromMaybe 0 . fmap getAddr <$> mem
    b3 = romFilePow2 "_build/bios/Bios.rom3" addr
    b2 = romFilePow2 "_build/bios/Bios.rom2" addr
    b1 = romFilePow2 "_build/bios/Bios.rom1" addr
    b0 = romFilePow2 "_build/bios/Bios.rom0" addr
    getAddr = \case
      InstrMem a    -> a
      DataMem a _ _ -> a

concat4
  :: KnownNat n
  => BitVector n
  -> BitVector n
  -> BitVector n
  -> BitVector n
  -> BitVector (4 * n)
concat4 b3 b2 b1 b0 = b3 ++# b2 ++# b1 ++# b0

--------------
-- Lion SOC --
--------------
lion :: HiddenClockResetEnable dom => Signal dom Rgb
lion = rgb toMem
  where
    FromCore toMem _ = core $ ToCore fromBios
    fromBios = bios toMem
    
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