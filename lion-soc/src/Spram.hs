{-|
Module      : Spram
Description : Lion SoC Single-port RAM
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com
-}

module Spram where

import Clash.Prelude
import Ice40.Spram
import Bus ( ToSpram(..) )

spram 
  :: HiddenClockResetEnable dom 
  => Signal dom ToSpram 
  -> Signal dom (BitVector 32)
spram toSpram = ram32k32 (spramAddress     <$> toSpram) 
                         (spramData        <$> toSpram)
                         (spramMask        <$> toSpram)
                         (spramWriteEnable <$> toSpram)

ram32k32
  :: HiddenClockResetEnable dom
  => Signal dom (BitVector 15) -- address
  -> Signal dom (BitVector 32) -- dataIn
  -> Signal dom (BitVector 8)  -- maskWrEn
  -> Signal dom Bit            -- wrEn
  -> Signal dom (BitVector 32) -- dataOut        
ram32k32 address dataIn maskWrEn wrEn = (++#) <$> dataOutH <*> dataOutL
  where
    dataOutH = ram32k16 address dataInH maskWrEnH wrEn
    dataOutL = ram32k16 address dataInL maskWrEnL wrEn
    (dataInH, dataInL)     = unbundle $ split <$> dataIn
    (maskWrEnH, maskWrEnL) = unbundle $ split <$> maskWrEn

ram32k16
  :: HiddenClockResetEnable dom
  => Signal dom (BitVector 15) -- address        
  -> Signal dom (BitVector 16) -- dataIn
  -> Signal dom (BitVector 4)  -- maskWrEn
  -> Signal dom Bit            -- wrEn
  -> Signal dom (BitVector 16) -- dataOut
ram32k16 address dataIn maskWrEn wrEn = mux addrMsbHigh' dataOutH dataOutL
  where
    dataOutH = ram16k16 address' dataIn maskWrEn wrEnH
    dataOutL = ram16k16 address' dataIn maskWrEn wrEnL
    address' = slice d13 d0 <$> address
    addrMsbHigh = (== high) . msb <$> address
    wrEnH = mux addrMsbHigh wrEn $ pure 0
    wrEnL = mux addrMsbHigh (pure 0) wrEn
    addrMsbHigh' = delay False addrMsbHigh

ram16k16
  :: HiddenClock dom
  => Signal dom (BitVector 14) -- address         
  -> Signal dom (BitVector 16) -- dataIn
  -> Signal dom (BitVector 4)  -- maskWrEn
  -> Signal dom Bit            -- wrEn
  -> Signal dom (BitVector 16) -- dataOut
ram16k16 address dataIn maskWrEn wrEn
  = spramPrim hasClock address dataIn maskWrEn wrEn (pure 1) (pure 0) (pure 0) (pure 1)
