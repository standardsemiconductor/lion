{-|
Module      : Bus
Description : Lion SoC Bus and Memory Map
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com
-}
module Bus where

import Clash.Prelude
import Lion.Core (ToMem(..), MemoryAccess(..))

---------
-- Bus --
---------
data Peripheral = Rom
                | Led
                | Uart
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

data BusIn (p :: Peripheral) where
  ToRom :: Unsigned 8 -- ^ ROM word address
        -> BusIn 'Rom -- ^ ROM access

  ToLed :: BitVector 4 -- ^ LED IP register address
        -> BitVector 8 -- ^ LED IP register write data
        -> Bool        -- ^ LED IP enable
        -> BusIn 'Led  -- ^ LED access

  ToUart :: BitVector 3         -- ^ UART mask
         -> Maybe (BitVector 8) -- ^ UART write value
         -> BusIn 'Uart         -- ^ UART access

data BusOut (p :: Peripheral) where
  FromRom  :: BitVector 32 -> BusOut 'Rom
  FromUart :: BitVector 32 -> BusOut 'Uart

romMap :: Maybe ToMem -> BusIn 'Rom
romMap = ToRom . wordAddr . maybe 0 memAddress
  where
    wordAddr :: BitVector 32 -> Unsigned 8
    wordAddr a = unpack $ slice d7 d0 $ a `shiftR` 2

uartMap :: Peripheral -> Maybe ToMem -> BusIn 'Uart
uartMap Uart (Just (ToMem DataMem _ msk wrM)) = ToUart (slice d2 d0 msk) $ slice d7 d0 <$> wrM
uartMap _ _ = ToUart 0 Nothing

ledMap :: Peripheral -> Maybe ToMem -> BusIn 'Led
ledMap Led (Just (ToMem _ _ $(bitPattern "..11") (Just d))) = ToLed (slice d11 d8 d) (slice d7 d0 d) True
ledMap _ _ = ToLed 0 0 False

selectPeripheral :: Maybe ToMem -> Peripheral
selectPeripheral Nothing = Rom
selectPeripheral (Just toMem)
  | checkRegion 10 = Rom
  | checkRegion  2 = Uart
  | otherwise      = Led
  where
    checkRegion :: Index 32 -> Bool
    checkRegion = bitToBool . (memAddress toMem !)

busMapOut :: BusOut 'Rom -> BusOut 'Uart -> Peripheral -> BitVector 32
busMapOut (FromRom fromBios) (FromUart fromUart) = \case
  Rom  -> fromBios
  Uart -> fromUart
  Led  -> 0
