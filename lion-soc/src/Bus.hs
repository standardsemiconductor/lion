{-|
Module      : Bus
Description : Lion SoC Bus and Memory Map
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com
-}
module Bus where

import Clash.Prelude
import Lion.Core (ToMem(..))

---------
-- Bus --
---------
-- | SoC Memory/Peripheral access bus
data Bus = Rom                     -- ^ rom access 
             (Unsigned 8)          -- ^ rom word address
         | Led                     -- ^ LED access 
             (BitVector 4)         -- ^ LED IP Register Address
             (BitVector 8)         -- ^ LED IP Register Write Data
         | Uart                    -- ^ UART access 
             (BitVector 3)         -- ^ UART mask
             (Maybe (BitVector 8)) -- ^ UART write value             
         | Spram                   -- ^ Single-port RAM access
             (BitVector 15)        -- ^ address
             (BitVector 32)        -- ^ dataIn
             (BitVector 8)         -- ^ mask write enable
             Bit                   -- ^ write enable
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

spramMap :: ToMem -> Maybe Bus
spramMap mem = Just $ Spram wordAddr dIn mskWrEn wrEn
  where
    wordAddr = slice d14 d0 $ getAddress mem `shiftR` 2
    (dIn, mskWrEn, wrEn) = case mem of
      DataMem _ msk (Just d) -> (d, expandMask msk, 1)
      _                      -> (0, 0, 0)
      where
        -- convert byte mask (4 bits) to nybble mask (8 bits)
        expandMask :: (KnownNat n, KnownNat m) => BitVector n -> BitVector (m * n)
        expandMask = concatBitVector# . map expandBit . bv2v
          where
            expandBit b
              | b == high = maxBound
              | otherwise = 0

romMap :: ToMem -> Maybe Bus
romMap = Just . Rom . wordAddr . getAddress
  where
    wordAddr :: BitVector 32 -> Unsigned 8
    wordAddr a = unpack $ slice d7 d0 $ a `shiftR` 2

uartMap :: ToMem -> Maybe Bus
uartMap = \case
  DataMem _ msk wrM -> Just $ Uart (slice d2 d0 msk) $ slice d7 d0 <$> wrM
  _ -> Nothing

ledMap :: ToMem -> Maybe Bus
ledMap = \case
  DataMem _ $(bitPattern "..11") (Just d) -> Just $ Led (slice d11 d8 d) (slice d7 d0 d)
  _ -> Nothing

busMapIn :: ToMem -> Maybe Bus
busMapIn toMem = case getAddress toMem of
  $(bitPattern "..............1.................") -> spramMap toMem -- spram
  $(bitPattern ".....................1..........") -> romMap   toMem -- rom
  $(bitPattern ".............................1..") -> uartMap  toMem -- uart
  _ -> ledMap toMem

busMapOut 
  :: Maybe Bus 
  -> BitVector 32  -- from bios
  -> BitVector 32  -- from uart
  -> BitVector 32  -- from spram
  -> BitVector 32
busMapOut busOut fromBios fromUart fromSpram = case busOut of
  Just (Rom _)    -> fromBios
  Just (Uart _ _) -> fromUart
  _               -> fromSpram


-------------
-- Utility --
-------------

getAddress :: ToMem -> BitVector 32
getAddress = \case
  InstrMem a     -> a
  DataMem  a _ _ -> a

