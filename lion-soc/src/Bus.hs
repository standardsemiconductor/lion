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
{-
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
-}

romMap :: Maybe ToMem -> Unsigned 8
romMap = unpack . slice d7 d0 . (`shiftR` 2) . maybe 0 memAddress 

data ToLed = ToLed 
  { ledAddress :: BitVector 4 
  , ledWrite   :: BitVector 8
  , ledEnable  :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

ledMap :: Maybe ToMem -> ToLed
ledMap = \case
  Just (ToMem _ 0 $(bitPattern "..11") (Just d)) 
    -> ToLed (slice d11 d8 d) (slice d7 d0 d) True
  _ -> ToLed 0 0 False 

data ToUart = ToUart
  { uartMask :: BitVector 3
  , uartWrite :: Maybe (BitVector 8)
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

uartMap :: Maybe ToMem -> ToUart
uartMap = \case
  Just (ToMem DataMem 0x4 mask wrM) -> ToUart (slice d2 d0 mask) $ slice d7 d0 <$> wrM
  _ -> ToUart 0x4 Nothing -- read status default

data ToSpram = ToSpram
  { spramAddress     :: BitVector 15
  , spramData        :: BitVector 32
  , spramMask        :: BitVector 8
  , spramWriteEnable :: Bit
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

spramMap :: Maybe ToMem -> ToSpram
spramMap = \case
  Just (ToMem _ addr@($(bitPattern "000000000000001.................")) mask wrM) 
    -> let wordAddr = slice d14 d0 $ addr `shiftR` 2
           (dIn, mskWrEn, wrEn) = case wrM of
             Just d  -> (d, expandMask mask, 1)
             Nothing -> (0, 0, 0)
       in ToSpram wordAddr dIn mskWrEn wrEn
  _ -> ToSpram 0 0 0 0
  where
    -- convert byte mask (4 bits) to nybble mask (8 bits)
    expandMask :: (KnownNat n, KnownNat m) => BitVector n -> BitVector (m * n)
    expandMask = concatBitVector# . map expandBit . bv2v
      where
        expandBit b
          | b == high = maxBound
          | otherwise = 0


--busMapIn :: ToMem -> Maybe Bus

{-
busMapIn (ToMem acc addr mask wrM)
  | checkBit 17                   = spramMap addr mask wrM
  | checkBit 10                   = Just $ Rom $ romAddress addr
  | checkBit  2 && acc == DataMem = Just $ Uart (slice d2 d0 mask) $ slice d7 d0 <$> wrM
  | mask .&. 0x3 /= 0             = ledMap <$> wrM
  | otherwise = Nothing
-}
{-
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
-}
{-

  where
    checkBit :: Index 32 -> Bool
    checkBit n = bitToBool $ addr ! n
-}
busMapOut 
  :: Maybe ToMem 
  -> BitVector 32  -- from spram
  -> BitVector 32  -- from bios
  -> BitVector 32  -- from uart
  -> BitVector 32
busMapOut mem fromSpram fromBios fromUart = case memAddress <$> mem of
  Just $(bitPattern "..............1.................") -> fromSpram
  Just $(bitPattern ".....................1..........") -> fromBios
  _ -> fromUart
{-
  Just (Spram _ _ _ _) -> fromSpram
  Just (Rom _)         -> fromBios
  _                    -> fromUart
-}
-------------
-- Utility --
-------------
{-
getAddress :: ToMem -> BitVector 32
getAddress = \case
  InstrMem a     -> a
  DataMem  a _ _ -> a
-}

