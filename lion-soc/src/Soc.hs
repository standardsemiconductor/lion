{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

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
import Control.Applicative ((<|>))
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

rgb :: HiddenClock dom => Signal dom (BusIn xl 'Led) -> Signal dom Rgb
rgb mem = rgbPrim "0b0" "0b111111" "0b111111" "0b111111" (pure 1) (pure 1) r g b
  where
    (r, g, b, _) = led (pure 1) wr addr en (pure True)
    (wr, addr, en) = unbundle $ mem <&> \case
      ToLed a d e -> (d, a, e)

----------
-- ROM --
----------
bios
  :: forall dom xl . HiddenClockResetEnable dom
  => Signal dom (BusIn xl 'Rom)
  -> Signal dom (BusOut xl 'Rom)
bios mem = fmap FromRom $ resize . concatBitVector# <$> bs
  where
    bs :: Signal dom (Vec (Div xl 8) (BitVector 8))
    bs = bundle $ (\ n -> romFilePow2 ("_build/bios/bios.rom" <|> show n) addr) <$> iterate (divSNat (SNat :: SNat xl) 8) succ (0 :: Int)
    addr = mem <&> \case
      ToRom a -> a

--------------
-- Lion SoC --
--------------
{-# NOINLINE lion32 #-}
lion32 :: HiddenClockResetEnable dom => Signal dom Bit -> FromSoc dom
lion32 = lion d32

lion :: forall dom xl . (HiddenClockResetEnable dom, 1 <= Div xl 8, 1025 <= (2^xl)) => SNat xl -> Signal dom Bit -> FromSoc dom
lion _ rxIn = FromSoc
  { rgbOut = fromRgb
  , txOut  = tx
  , spiIO  = spiio
  }
  where
    config :: CoreConfig 0x400 'Hard
    config = CoreConfig PipeConfig
    fromBios :: _ (_ xl _)
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
topEntity = withClockResetEnable clk latticeRst enableGen lion32
  where
    clk = hf12Mhz (pure True :: Signal System Bool)
                  (pure True :: Signal System Bool)
makeTopEntityWithName 'topEntity "Soc"
