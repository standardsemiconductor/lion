{-|
Module      : Spram
Description : Lion SoC Single-port RAM
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com
-}

module Spram where

import Clash.Prelude
import Data.Functor ( (<&>) )
import Ice40.Spram
import Bus ( Bus(Spram) )

spram 
  :: HiddenClockResetEnable dom 
  => Signal dom (Maybe Bus) 
  -> Signal dom (BitVector 32)
spram busInM = ram32k32 address dataIn maskWrEn wrEn
  where
    (address, dataIn, maskWrEn, wrEn) = unbundle $ busInM <&> \case
      Just (Spram a dIn mskWrEn en) -> (a, dIn, mskWrEn, en)
      _ -> (0, 0, 0, 0)

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
    (dataInH, dataInL) = unbundle $ split <$> dataIn
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
