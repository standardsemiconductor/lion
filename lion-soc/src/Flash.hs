{-|
Module      : Flash
Description : Lion SoC SPI flash memory peripheral
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com
-}

module Flash where

import Clash.Prelude
import Ice40.Spi
import Bus ( Bus (Flash) )

data SpiIO = SpiIO ("biwo" ::: Bit)
                   ("bowi" ::: Bit)
                   ("wck"  ::: Bit)
                   ("cs"   ::: Bit)
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

flash
  :: HiddenClock dom
  => Signal dom (Maybe Bus)
  -> Unbundled dom (SpiIO, BitVector 32)
flash busIn = (spiIO, fromFlash)
  where
    fromSb = sysBus toSb
   
    spiIO = SpiIO <$> biwo <*> bowi <*> wck <*> cs
    (biwo, bi) = biwoIO
    bowi       = bowiIO boe    bo
    wck        = wckIO  wckoe  wcko
    cs         = csIO   bcsnoe bcsno

    (sbdato, sbacko, _, _, _, _, bo, boe, wcko, wckoe, bcsno, bcsnoe) 
      = spi "0b0000"
            (getSbrwi  <$> fromSb)
            (getSbstbi <$> fromSb)
            (getSbadri <$> fromSb)
            (getSbdati <$> fromSb)
            bi
            (pure 0)
            (pure 0)
            (pure 0)

sysBus :: Signal dom (Maybe Bus) -> Unbundled (FromSb, BitVector 32)
sysBus = _

biwoIO :: HiddenClock dom => Unbundled dom (Bit, Bit)
biwoIO = (biwo, bi)
  where
    (biwo, bi, _) = io PinInput
                       PinNoOutput
                       0 -- pullUp
                       0 -- negTrigger
                       SBLVCMOS
                       0
                       0
                       hasClock
                       hasClock
                       0
                       0
                       0

bowiIO :: HiddenClock dom => Signal dom Bit -> Signal dom Bit -> Signal dom Bit
bowiIO boe boe = bowi
  where
    (bowi, _, _) = io
                     PinInput
                     PinOutputTristate
                     0 -- pullup
                     0 -- negTrigger
                     SBLVCMOS
                     0 -- latchInputValue
                     0 -- clock enable
                     hasClock
                     hasClock
                     boe -- output enable
                     bo  -- dOut0
                     0

wckIO :: HiddenClock dom => Signal dom Bit -> Signal dom Bit -> Signal dom Bit
wckIO wckoe wcko = wck
  where
    (wck, _, _) = io PinInput
                     PinOutputTristate
                     1 -- pullUp
                     0 -- negTrigger
                     SBLVCMOS
                     0 -- latchInputValue
                     0 -- clock enable
                     hasClock
                     hasClock
                     wckoe
                     wcko
                     0

csIO 
  :: HiddenClock dom 
  => Signal dom (BitVector 4) 
  -> Signal dom (BitVector 4) 
  -> Signal dom Bit
csIO bcsnoe bcsno = cs
  where
    (cs, _, _) = io PinInput
                    PinOutputTristate
                    1 -- pullUp
                    0 -- negTrigger
                    SBLVCMOS
                    0 -- latch input value
                    0 -- clock enable
                    hasClock
                    hasClock
                    ((!(3 :: Index 4)) <$> bcsnoe) -- output enable
                    ((!(3 :: Index 4)) <$> bcsno)  -- dOut0
                    0 -- dOut1
                
                     