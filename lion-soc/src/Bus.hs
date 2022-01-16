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

data BusIn xl (p :: Peripheral) where
  ToRom :: Unsigned 8 -- ^ ROM word address
        -> BusIn xl 'Rom -- ^ ROM access

  ToLed :: BitVector 4 -- ^ LED IP register address
        -> BitVector 8 -- ^ LED IP register write data
        -> Bool        -- ^ LED IP enable
        -> BusIn xl 'Led  -- ^ LED access

  ToUart :: BitVector 3         -- ^ UART mask
         -> Maybe (BitVector 8) -- ^ UART write value
         -> BusIn xl 'Uart         -- ^ UART access

  ToSpram :: BitVector 15 -- ^ SPRAM address
          -> BitVector xl -- ^ SPRAM dataIn
          -> BitVector (Div xl 4) -- ^ SPRAM mask write enable
          -> Bit          -- ^ SPRAM write enable
          -> BusIn xl 'Spram -- ^ SPRAM access

  ToSpi :: BitVector (Div xl 8) -- ^ SPI mask
        -> Maybe (BitVector xl) -- ^ SPI read/write, read=Nothing, write=Just value
        -> BusIn xl 'Spi           -- ^ SPI access

data BusOut xl (p :: Peripheral) where
  FromRom   :: BitVector xl -> BusOut xl 'Rom
  FromUart  :: BitVector xl -> BusOut xl 'Uart
  FromSpram :: BitVector xl -> BusOut xl 'Spram
  FromSpi   :: BitVector xl -> BusOut xl 'Spi

romMap :: forall xl . KnownNat xl => Maybe (ToMem xl) -> BusIn xl 'Rom
romMap = ToRom . wordAddr . maybe 0 memAddress
  where
    wordAddr :: BitVector xl -> Unsigned 8
    wordAddr a = unpack $ resize $ a `shiftR` 2

uartMap :: KnownNat xl => Peripheral -> Maybe (ToMem xl) -> BusIn xl 'Uart
uartMap Uart (Just (ToMem DataMem _ msk wrM)) = ToUart (resize msk) $ resize <$> wrM
uartMap _ _ = ToUart 0 Nothing

ledMap :: KnownNat xl => Peripheral -> Maybe (ToMem xl) -> BusIn xl 'Led
ledMap Led (Just (ToMem _ _ $(bitPattern "..11") (Just d'))) = ToLed (slice d11 d8 d) (slice d7 d0 d) True
  where d = resize d' :: BitVector 12
ledMap _ _ = ToLed 0 0 False

spramMap :: KnownNat xl => Peripheral -> Maybe (ToMem xl) -> BusIn xl 'Spram
spramMap _      Nothing    = ToSpram 0 0 0 0
spramMap periph (Just mem) = case (periph, memWrite mem) of
  (Spram, Just wr) -> ToSpram wordAddr wr (resize nybMask) 1
  _                -> ToSpram wordAddr 0 0 0
  where
    wordAddr = resize $ memAddress mem `shiftR` 2
    nybMask = concatBitVector# $ map expandBit $ bv2v $ memByteMask mem
      where
        expandBit :: Bit -> BitVector 2
        expandBit b
          | b == high = 0b11
          | otherwise = 0b00

spiMap :: KnownNat xl => Peripheral -> Maybe (ToMem xl) -> BusIn xl 'Spi
spiMap Spi (Just (ToMem DataMem _ mask wrM)) = ToSpi mask wrM
spiMap _   _                                 = ToSpi 0    Nothing 

selectPeripheral :: forall xl . KnownNat xl => Maybe (ToMem xl) -> Peripheral
selectPeripheral Nothing = Rom
selectPeripheral (Just toMem)
  | checkRegion 17 = Spram
  | checkRegion 10 = Rom
  | checkRegion  3 = Spi
  | checkRegion  2 = Uart
  | otherwise      = Led
  where
    checkRegion :: Index xl -> Bool
    checkRegion = bitToBool . (memAddress toMem !)

busMapOut 
  :: KnownNat xl
  => BusOut xl 'Rom 
  -> BusOut xl 'Uart 
  -> BusOut xl 'Spram 
  -> BusOut xl 'Spi
  -> Peripheral 
  -> BitVector xl
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
