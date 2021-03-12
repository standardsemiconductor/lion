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

spramMap :: BitVector 32 -> BitVector 4 -> Maybe (BitVector 32) -> Maybe Bus
spramMap addr mask wrM = Just $ Spram wordAddr dIn mskWrEn wrEn
  where
    wordAddr = slice d14 d0 $ addr `shiftR` 2
    (dIn, mskWrEn, wrEn) = case wrM of
      Just d  -> (d, expandMask mask, 1)
      Nothing -> (0, 0, 0)
      where
        -- convert byte mask (4 bits) to nybble mask (8 bits)
        expandMask :: (KnownNat n, KnownNat m) => BitVector n -> BitVector (m * n)
        expandMask = concatBitVector# . map expandBit . bv2v
          where
            expandBit b
              | b == high = maxBound
              | otherwise = 0

romAddress :: BitVector 32 -> Unsigned 8
romAddress a = unpack $ slice d7 d0 $ a `shiftR` 2

ledMap :: BitVector 32 -> Bus
ledMap wr = Led (slice d11 d8 wr) (slice d7 d0 wr)

busMapIn :: ToMem -> Maybe Bus

{-
busMapIn (ToMem acc addr mask wrM)
  | checkBit 17                   = spramMap addr mask wrM
  | checkBit 10                   = Just $ Rom $ romAddress addr
  | checkBit  2 && acc == DataMem = Just $ Uart (slice d2 d0 mask) $ slice d7 d0 <$> wrM
  | mask .&. 0x3 /= 0             = ledMap <$> wrM
  | otherwise = Nothing
-}

busMapIn = \case
  ToMem _ addr@($(bitPattern "..............1.................")) mask wrM -- spram
    -> spramMap addr mask wrM
  ToMem _ addr@($(bitPattern ".....................1..........")) _ _ -- rom
    -> Just $ Rom $ romAddress addr
  ToMem DataMem $(bitPattern ".............................1..") mask wrM -- uart
    -> Just $ Uart (slice d2 d0 mask) $ slice d7 d0 <$> wrM
  ToMem _ _ $(bitPattern "..11") (Just wr) 
    -> Just $ ledMap wr
  _ -> Nothing
{-

  where
    checkBit :: Index 32 -> Bool
    checkBit n = bitToBool $ addr ! n
-}
busMapOut 
  :: Maybe Bus 
  -> BitVector 32  -- from spram
  -> BitVector 32  -- from bios
  -> BitVector 32  -- from uart
  -> BitVector 32
busMapOut busOut fromSpram fromBios fromUart = case busOut of
  Just (Spram _ _ _ _) -> fromSpram
  Just (Rom _)         -> fromBios
  _                    -> fromUart

-------------
-- Utility --
-------------
{-
getAddress :: ToMem -> BitVector 32
getAddress = \case
  InstrMem a     -> a
  DataMem  a _ _ -> a
-}
