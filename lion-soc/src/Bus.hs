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
                | Spram
                | Spi
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

  ToSpram :: BitVector 15 -- ^ SPRAM address
          -> BitVector 32 -- ^ SPRAM dataIn
          -> BitVector 8  -- ^ SPRAM mask write enable
          -> Bit          -- ^ SPRAM write enable
          -> BusIn 'Spram -- ^ SPRAM access

  ToSpi :: BitVector 4          -- ^ SPI mask
        -> Maybe (BitVector 32) -- ^ SPI read/write, read=Nothing, write=Just value
        -> BusIn 'Spi           -- ^ SPI access

data BusOut (p :: Peripheral) where
  FromRom   :: BitVector 32 -> BusOut 'Rom
  FromUart  :: BitVector 32 -> BusOut 'Uart
  FromSpram :: BitVector 32 -> BusOut 'Spram
  FromSpi   :: BitVector 32 -> BusOut 'Spi

romMap :: Maybe (ToMem 32) -> BusIn 'Rom
romMap = ToRom . wordAddr . maybe 0 memAddress
  where
    wordAddr :: BitVector 32 -> Unsigned 8
    wordAddr a = unpack $ slice d7 d0 $ a `shiftR` 2

uartMap :: Peripheral -> Maybe (ToMem 32) -> BusIn 'Uart
uartMap Uart (Just (ToMem DataMem _ msk wrM)) = ToUart (slice d2 d0 msk) $ slice d7 d0 <$> wrM
uartMap _ _ = ToUart 0 Nothing

ledMap :: Peripheral -> Maybe (ToMem 32) -> BusIn 'Led
ledMap Led (Just (ToMem _ _ $(bitPattern "..11") (Just d))) = ToLed (slice d11 d8 d) (slice d7 d0 d) True
ledMap _ _ = ToLed 0 0 False

spramMap :: Peripheral -> Maybe (ToMem 32) -> BusIn 'Spram
spramMap _      Nothing    = ToSpram 0 0 0 0
spramMap periph (Just mem) = case (periph, memWrite mem) of
  (Spram, Just wr) -> ToSpram wordAddr wr nybMask 1
  _                -> ToSpram wordAddr 0 0 0
  where
    wordAddr = slice d14 d0 $ memAddress mem `shiftR` 2
    nybMask = concatBitVector# $ map expandBit $ bv2v $ memByteMask mem
      where
        expandBit :: Bit -> BitVector 2
        expandBit b
          | b == high = 0b11
          | otherwise = 0b00

spiMap :: Peripheral -> Maybe (ToMem 32) -> BusIn 'Spi
spiMap Spi (Just (ToMem DataMem _ mask wrM)) = ToSpi mask wrM
spiMap _   _                                 = ToSpi 0    Nothing 

selectPeripheral :: Maybe (ToMem 32) -> Peripheral
selectPeripheral Nothing = Rom
selectPeripheral (Just toMem)
  | checkRegion 17 = Spram
  | checkRegion 10 = Rom
  | checkRegion  3 = Spi
  | checkRegion  2 = Uart
  | otherwise      = Led
  where
    checkRegion :: Index 32 -> Bool
    checkRegion = bitToBool . (memAddress toMem !)

busMapOut 
  :: BusOut 'Rom 
  -> BusOut 'Uart 
  -> BusOut 'Spram 
  -> BusOut 'Spi
  -> Peripheral 
  -> BitVector 32
busMapOut (FromRom   fromBios) 
          (FromUart  fromUart) 
          (FromSpram fromSpram) 
          (FromSpi   fromSpi)
  = \case
    Rom   -> fromBios
    Uart  -> fromUart
    Led   -> 0
    Spram -> fromSpram
    Spi   -> fromSpi
