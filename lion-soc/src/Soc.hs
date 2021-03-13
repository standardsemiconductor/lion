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
import Ice40.Clock
import Ice40.Osc ( hf12Mhz )
import Ice40.Rgb
import Ice40.Led
import Lion.Core
import Bus   ( busMapOut, ToLed(..), ledMap, romMap, uartMap, spramMap)
import Uart  ( uart )
import Spram ( spram )

data FromSoc dom = FromSoc
  { rgbOut :: "led"     ::: Signal dom Rgb
  , txOut  :: "uart_tx" ::: Signal dom Bit
  }

---------
-- RGB --
---------
type Rgb = ("red" ::: Bit, "green" ::: Bit, "blue" ::: Bit)

rgb :: HiddenClock dom => Signal dom ToLed -> Signal dom Rgb
rgb toLed = rgbPrim "0b0" "0b111111" "0b111111" "0b111111" (pure 1) (pure 1) r g b
  where
    (r, g, b, _) = led (pure 1) 
                       (ledWrite   <$> toLed) 
                       (ledAddress <$> toLed) 
                       (ledEnable  <$> toLed) 
                       (pure True)

----------
-- ROM --
----------
bios
  :: HiddenClockResetEnable dom
  => Signal dom (Unsigned 8)
  -> Signal dom (BitVector 32)
bios addr = concat4 <$> b3 <*> b2 <*> b1 <*> b0
  where
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
{-# NOINLINE lion #-}
lion :: HiddenClockResetEnable dom => Signal dom Bit -> FromSoc dom
lion rx = FromSoc
  { rgbOut = fromRgb
  , txOut  = tx
  }
  where
    config = defaultCoreConfig{ pipeConfig = defaultPipeConfig{ startPC = 0x400 } }
    fromSpram      = spram   $ spramMap <$> fromCore
    fromBios       = bios    $ romMap   <$> fromCore
    fromRgb        = rgb     $ ledMap   <$> fromCore
    (tx, fromUart) = uart rx $ uartMap  <$> register Nothing fromCore
    fromCore = toMem $ core config $
      busMapOut <$> register Nothing fromCore
                <*> fromSpram
                <*> fromBios 
                <*> fromUart

----------------
-- Top Entity --
----------------
{-# NOINLINE topEntity #-}
topEntity :: "uart_rx" ::: Signal Lattice12Mhz Bit -> FromSoc Lattice12Mhz
topEntity = withClockResetEnable clk latticeRst enableGen lion
  where
    clk = hf12Mhz (pure True :: Signal System Bool)
                  (pure True :: Signal System Bool)
makeTopEntityWithName 'topEntity "Soc"
