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

flash
  :: HiddenClock dom
  => Signal dom (Maybe Bus)
  -> Signal dom (BitVector 32)
flash busIn = _
  where
    fromSb = sysBus toSb
    (sbdato, sbacko, _, _, wo, woe, bo, boe, wcko, wckoe, bcsno, bcsnoe) 
      = spi "0b0000"
            (getSbrwi  <$> fromSb)
            (getSbstbi <$> fromSb)
            (getSbadri <$> fromSb)
            (getSbdati <$> fromSb)
            bi
            wi -- ??
            wcki -- ??
            wcsni -- ??