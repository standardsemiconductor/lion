{-# LANGUAGE ApplicativeDo #-}

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

spramMap :: ToMem -> Bus
spramMap mem = maybe spramRead spramWrite $ memWrite mem
  where
    spramRead     = Spram wordAddr 0 0 0
    spramWrite wr = Spram wordAddr wr nybMask 1

    wordAddr = slice d14 d0 $ memAddress mem `shiftR` 2

    -- convert byte mask (4 bits) to nybble mask (8 bits)
    nybMask = concatBitVector# $ map expandBit $ bv2v $ memByteMask mem
      where
        expandBit :: Bit -> BitVector 2
        expandBit b
          | b == high = 0b11
          | otherwise = 0b00

romMap :: ToMem -> Bus
romMap = Rom .  unpack . slice d7 d0 . (`shiftR` 2) . memAddress

uartMap :: ToMem -> Bus
uartMap mem = case memAccess mem of
  DataMem -> Uart mask wrM
  _       -> Uart 0 Nothing
  where
    mask = slice d2 d0 $ memByteMask mem
    wrM  = slice d7 d0 <$> memWrite mem

ledMap :: ToMem -> Bus
ledMap = maybe (Rom 0) led . memWrite
  where
    led :: BitVector 32 -> Bus
    led d = Led (slice d11 d8 d) (slice d7 d0 d)

busMapIn :: Maybe ToMem -> Bus
busMapIn Nothing = Rom 0
busMapIn (Just toMem)
  | checkRegion 17 = spramMap toMem
  | checkRegion 10 = romMap   toMem
  | checkRegion  2 = uartMap  toMem
  | otherwise      = ledMap   toMem
  where
    checkRegion :: Index 32 -> Bool
    checkRegion n = bitToBool $ memAddress toMem ! n

busMapOut 
  :: Bus 
  -> BitVector 32
  -> BitVector 32
  -> BitVector 32
  -> BitVector 32
busMapOut busOut fromSpram fromBios fromUart = case busOut of
  Spram{} -> fromSpram
  Rom{}   -> fromBios
  Uart{}  -> fromUart
  Led{}   -> 0