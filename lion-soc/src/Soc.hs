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
import Ice40.Clock
import Ice40.Osc ( hf12Mhz )
import Ice40.Rgb
import Ice40.Led
import Lion.Core
import Bus  ( busMapIn, busMapOut, Bus(Rom, Led))
import Uart ( uart )

data FromSoc dom = FromSoc
  { rgbOut :: "led"     ::: Signal dom Rgb
  , txOut  :: "uart_tx" ::: Signal dom Bit
  }

---------
-- RGB --
---------
type Rgb = ("red" ::: Bit, "green" ::: Bit, "blue" ::: Bit)

rgb :: HiddenClock dom => Signal dom Bus -> Signal dom Rgb
rgb mem = rgbPrim "0b0" "0b111111" "0b111111" "0b111111" (pure 1) (pure 1) r g b
  where
    (r, g, b, _) = led (pure 1) wr addr en (pure True)
    (wr, addr, en) = unbundle $ mem <&> \case
      Led a d -> (d, a, True )
      _       -> (0, 0, False)

----------
-- ROM --
----------
bios
  :: HiddenClockResetEnable dom
  => Signal dom Bus
  -> Signal dom (BitVector 32)
bios mem = concat4 <$> b3 <*> b2 <*> b1 <*> b0
  where
    b3 = romFilePow2 "_build/bios/bios.rom3" addr
    b2 = romFilePow2 "_build/bios/bios.rom2" addr
    b1 = romFilePow2 "_build/bios/bios.rom1" addr
    b0 = romFilePow2 "_build/bios/bios.rom0" addr
    addr = mem <&> \case
      (Rom a) -> a
      _       -> 0

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
{-# NOINLINE lion #-}
lion :: HiddenClockResetEnable dom => Signal dom Bit -> FromSoc dom
lion rxIn = FromSoc
  { rgbOut = fromRgb
  , txOut  = tx
  }
  where
    config = defaultCoreConfig{ pipeConfig = defaultPipeConfig{ startPC = 0x400 } }
    fromBios       = bios      busIn
    fromRgb        = rgb $ register (Rom 0) busIn
    (tx, fromUart) = uart rxIn busIn
    busIn = fmap busMapIn $ toMem $ core config $ 
      busMapOut <$> register (Rom 0) busIn
                <*> fromBios 
                <*> fromUart

----------------
-- Top Entity --
----------------
{-# NOINLINE topEntity #-}
topEntity 
  :: "uart_rx" ::: Signal Lattice12Mhz Bit
  -> FromSoc Lattice12Mhz
topEntity = withClockResetEnable clk latticeRst enableGen lion
  where
    clk = hf12Mhz (pure True :: Signal System Bool)
                  (pure True :: Signal System Bool)
makeTopEntityWithName 'topEntity "Soc"
