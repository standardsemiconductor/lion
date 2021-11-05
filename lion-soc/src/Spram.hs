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
import Bus

spram 
  :: (HiddenClockResetEnable dom, KnownNat xl)
  => Signal dom (BusIn xl 'Spram)
  -> Signal dom (BusOut xl 'Spram)
spram busIn = FromSpram <$> ram32k addr dat msk we
  where
    (addr, dat, msk, we) = unbundle $ busIn <&> \case
      ToSpram a d m w -> (a, d, m, w)

ram32k :: forall dom xl
   . (HiddenClockResetEnable dom, KnownNat xl)
  => Signal dom (BitVector 15) -- address
  -> Signal dom (BitVector xl) -- dataIn
  -> Signal dom (BitVector (Div xl 4))  -- maskWrEn
  -> Signal dom Bit            -- wrEn
  -> Signal dom (BitVector xl) -- dataOut        
ram32k address dataIn maskWrEn wrEn = resize . concatBitVector# <$> sequenceA dataOuts
  where
    dataOuts :: Vec (Div xl 16) (Signal dom (BitVector 16))
    dataOuts = (\ u v -> ram32k16 address u v wrEn) <$> dataIns <*> maskWrEns
    dataIns = unbundle $ unconcatBitVector# . resize <$> dataIn
    maskWrEns = unbundle $ unconcatBitVector# . resize <$> maskWrEn

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