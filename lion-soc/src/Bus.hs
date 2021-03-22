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
-- | SoC Memory/Peripheral access bus
{-
data Bus = Rom            -- ^ ROM access 
             (Unsigned 8) -- ^ ROM word address
         | Led             -- ^ LED access 
             (BitVector 4) -- ^ LED IP Register Address
             (BitVector 8) -- ^ LED IP Register Write Data
         | Uart                    -- ^ UART access 
             (BitVector 3)         -- ^ UART mask
             (Maybe (BitVector 8)) -- ^ UART write value             
         | Spram            -- ^ SPRAM access
             (BitVector 15) -- ^ SPRAM address
             (BitVector 32) -- ^ SPRAM dataIn
             (BitVector 8)  -- ^ SPRAM mask write enable
             Bit            -- ^ SPRAM write enable
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
-}
data Peripheral = Rom
                | Led
                | Uart
                | Spram
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

data BusIn (p :: Peripheral) where
  ToRom :: Unsigned 8 -- ^ ROM word address
        -> BusIn 'Rom -- ^ ROM access
  ToLed :: BitVector 4 -- ^ LED IP Register Address
        -> BitVector 8 -- ^ LED IP Register Write Data
        -> Bool        -- ^ LED enable
        -> BusIn 'Led  -- ^ LED access
  ToUart :: BitVector 3         -- ^ UART mask
         -> Maybe (BitVector 8) -- ^ UART write value
         -> BusIn 'Uart         -- ^ UART access
  ToSpram :: BitVector 15 -- ^ SPRAM address
          -> BitVector 32 -- ^ SPRAM dataIn
          -> BitVector 8  -- ^ SPRAM mask write enable
          -> Bit          -- ^ SPRAM write enable
          -> BusIn 'Spram -- ^ SPRAM access

data BusOut (p :: Peripheral) where
  FromRom   :: BitVector 32 -> BusOut 'Rom
  FromUart  :: BitVector 32 -> BusOut 'Uart
  FromSpram :: BitVector 32 -> BusOut 'Spram

spramMap :: Maybe ToMem -> BusIn 'Spram
spramMap Nothing    = ToSpram 0 0 0 0
spramMap (Just mem) = maybe spramRead spramWrite $ memWrite mem
  where
    spramRead     = ToSpram wordAddr 0 0 0
    spramWrite wr = ToSpram wordAddr wr nybMask 1

    wordAddr = slice d14 d0 $ memAddress mem `shiftR` 2

    -- convert byte mask (4 bits) to nybble mask (8 bits)
    nybMask = concatBitVector# $ map expandBit $ bv2v $ memByteMask mem
      where
        expandBit :: Bit -> BitVector 2
        expandBit b
          | b == high = 0b11
          | otherwise = 0b00

romMap :: Maybe ToMem -> BusIn 'Rom
romMap = ToRom .  unpack . slice d7 d0 . (`shiftR` 2) . maybe 0 memAddress

uartMap :: Maybe ToMem -> BusIn 'Uart
uartMap Nothing    = ToUart 0 Nothing
uartMap (Just mem) = case memAccess mem of
  DataMem  -> ToUart mask wrM
  InstrMem -> ToUart 0 Nothing
  where
    mask = slice d2 d0 $ memByteMask mem
    wrM  = slice d7 d0 <$> memWrite mem

ledMap :: Maybe ToMem -> BusIn 'Led
ledMap = maybe (ToLed 0 0 False) led . (memWrite =<<)
  where
    led :: BitVector 32 -> BusIn 'Led
    led d = ToLed (slice d11 d8 d) (slice d7 d0 d) True

{-
busMapIn :: Maybe ToMem -> Bus
busMapIn Nothing = Rom 0
busMapIn (Just toMem)
  | checkRegion 17 = spramMap toMem
  | checkRegion 10 = romMap   toMem
  | checkRegion 2  = uartMap  toMem
  | otherwise      = ledMap   toMem
  where
    checkRegion :: Index 32 -> Bool
    checkRegion n = bitToBool $ memAddress toMem ! n
-}
selectRegion :: Maybe ToMem -> Peripheral
selectRegion Nothing = Rom
selectRegion (Just toMem)
  | checkRegion 17 = Spram
  | checkRegion 10 = Rom
  | checkRegion 2  = Uart
  | otherwise      = Led
  where
    checkRegion :: Index 32 -> Bool
    checkRegion n = bitToBool $ memAddress toMem ! n

busMapOut 
  :: BusOut 'Spram
  -> BusOut 'Rom
  -> BusOut 'Uart
  -> Peripheral
  -> BitVector 32
busMapOut (FromSpram fromSpram) (FromRom fromBios) (FromUart fromUart) = \case
  Rom   -> fromBios
  Led   -> 0
  Uart  -> fromUart
  Spram -> fromSpram  


