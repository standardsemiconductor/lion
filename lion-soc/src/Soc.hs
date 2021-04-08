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
import Bus
import Uart ( uart )
import Spram
import qualified Spi as S

data FromSoc dom = FromSoc
  { rgbOut :: "led"     ::: Signal dom Rgb
  , txOut  :: "uart_tx" ::: Signal dom Bit
  , spiIO  :: "spi"     ::: Signal dom S.SpiIO
  }

---------
-- RGB --
---------
type Rgb = ("red" ::: Bit, "green" ::: Bit, "blue" ::: Bit)

rgb :: HiddenClock dom => Signal dom (BusIn 'Led) -> Signal dom Rgb
rgb mem = rgbPrim "0b0" "0b111111" "0b111111" "0b111111" (pure 1) (pure 1) r g b
  where
    (r, g, b, _) = led (pure 1) wr addr en (pure True)
    (wr, addr, en) = unbundle $ mem <&> \case
      ToLed a d e -> (d, a, e)

----------
-- ROM --
----------
bios
  :: HiddenClockResetEnable dom
  => Signal dom (BusIn 'Rom)
  -> Signal dom (BusOut 'Rom)
bios mem = fmap FromRom $ concat4 <$> b3 <*> b2 <*> b1 <*> b0
  where
    b3 = romFilePow2 "_build/bios/bios.rom3" addr
    b2 = romFilePow2 "_build/bios/bios.rom2" addr
    b1 = romFilePow2 "_build/bios/bios.rom1" addr
    b0 = romFilePow2 "_build/bios/bios.rom0" addr
    addr = mem <&> \case
      ToRom a -> a

concat4
  :: KnownNat n
  => BitVector n
  -> BitVector n
  -> BitVector n
  -> BitVector n
  -> BitVector (4 * n)
concat4 b3 b2 b1 b0 = b3 ++# b2 ++# b1 ++# b0

--------------
-- Lion SoC --
--------------
{-# NOINLINE lion #-}
lion :: HiddenClockResetEnable dom => Signal dom Bit -> FromSoc dom
lion rxIn = FromSoc
  { rgbOut = fromRgb
  , txOut  = tx
  , spiIO  = spiio
  }
  where
    config :: CoreConfig 0x400 'Hard
    config = CoreConfig PipeConfig
    fromBios         = bios      $ romMap   <$> fromCore
    fromRgb          = rgb       $ ledMap   <$> peripheral <*> fromCore
    (tx, fromUart)   = uart rxIn $ uartMap  <$> peripheral <*> fromCore
    fromSpram        = spram     $ spramMap <$> peripheral <*> fromCore
    (spiio, fromSpi) = S.spi     $ spiMap   <$> peripheral <*> fromCore
    peripheral = selectPeripheral <$> fromCore
    fromCore = toMem $ core config $ 
      busMapOut <$> fromBios 
                <*> fromUart
                <*> fromSpram
                <*> fromSpi
                <*> register Rom peripheral

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
