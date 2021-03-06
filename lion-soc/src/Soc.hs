{-|
Module      : Soc
Description : Lion SoC on the VELDT
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com
-}

module Soc where

import Clash.Prelude
import Clash.Annotations.TH
import Data.Functor ( (<&>) )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( First(..) )
import Ice40.Clock
import Ice40.Rgb
import Ice40.Led
import Lion.Core (FromCore(..), defaultPipeConfig, core)
import Bus
import Uart

data FromSoc dom = FromSoc
  { rgbOut :: "led"  ::: Signal dom Rgb
  , txOut  :: "uart" ::: Signal dom Bit
  }

---------
-- RGB --
---------
type Rgb = ("red" ::: Bit, "green" ::: Bit, "blue" ::: Bit)

rgb :: HiddenClock dom => Signal dom (Maybe Bus) -> Signal dom Rgb
rgb mem = rgbPrim "0b0" "0b111111" "0b111111" "0b111111" (pure 1) (pure 1) r g b
  where
    (wr, addr, en) = unbundle $ mem <&> \case
      Just (Bus 
              $(bitPattern "000000000000000000000001000000..")
              $(bitPattern "0011")
              (Just d)
           ) -> (slice d7 d0 d, slice d11 d8 d, True)
      _ -> (0, 0, False)
    (r, g, b, _) = led (pure 1) wr addr en (pure True)

----------
-- BIOS --
----------
bios
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe Bus)
  -> Signal dom (BitVector 32)
bios mem = concat4 <$> b3 <*> b2 <*> b1 <*> b0
  where
    addr = unpack . slice d7 d0 . (`shiftR` 2) . fromMaybe 0 . fmap busAddr <$> mem
    b3 = romFilePow2 "_build/bios/bios.rom3" addr
    b2 = romFilePow2 "_build/bios/bios.rom2" addr
    b1 = romFilePow2 "_build/bios/bios.rom1" addr
    b0 = romFilePow2 "_build/bios/bios.rom0" addr

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
lion :: HiddenClockResetEnable dom => Signal dom Bit -> FromSoc dom
lion rxIn = FromSoc
  { rgbOut = fromRgb
  , txOut  = _tx <$> fromUart
  }
  where
    fromBios = bios fromCore
    fromRgb  = rgb  fromCore 
    fromUart = uart fromCore rxIn
    fromCore = (fmap.fmap) mkBus $ toMem $ core defaultPipeConfig $ 
      case delay Nothing (fmap busAddr <$> fromCore) of
        $(bitPattern "000000000000000000000000........") -> fromBios
        $(bitPattern "000000000000000000000001000000..") -> fromRgb
        $(bitPattern "000000000000000000000001000001..") -> fromUart
        _ -> pure 0
      
----------------
-- Top Entity --
----------------
{-# NOINLINE topEntity #-}
topEntity 
  :: "clk" ::: Clock Lattice12Mhz 
  -> "uart_rx" ::: Signal Lattice12Mhz Bit
  -> FromSoc Lattice12Mhz
topEntity clk = withClockResetEnable clk latticeRst enableGen lion
makeTopEntityWithName 'topEntity "Soc"
