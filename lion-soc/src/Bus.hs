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
spramMap (ToMem _ addr mask wrM) = let wordAddr = slice d14 d0 $ addr `shiftR` 2
                                       (dIn, mskWrEn, wrEn) = case wrM of
                                         Just d  -> (d, expandMask mask, 1)
                                         Nothing -> (0, 0, 0)
                                   in Spram wordAddr dIn mskWrEn wrEn
  where
    -- convert byte mask (4 bits) to nybble mask (8 bits)
    expandMask :: (KnownNat n, KnownNat m) => BitVector n -> BitVector (m * n)
    expandMask = concatBitVector# . map expandBit . bv2v
      where
        expandBit b
          | b == high = maxBound
          | otherwise = 0

romMap :: ToMem -> Bus
romMap = Rom . wordAddr . memAddress
  where
    wordAddr :: BitVector 32 -> Unsigned 8
    wordAddr a = unpack $ slice d7 d0 $ a `shiftR` 2

uartMap :: ToMem -> Bus
uartMap (ToMem _ _ msk wrM) = Uart (slice d2 d0 msk) $ slice d7 d0 <$> wrM

ledMap :: ToMem -> Bus
ledMap = \case
  ToMem _ _ $(bitPattern "..11") (Just d) -> Led (slice d11 d8 d) (slice d7 d0 d)
  _ -> Rom 0

busMapIn :: Maybe ToMem -> Bus
busMapIn Nothing = Rom 0
busMapIn (Just toMem)
  | checkRegion 17              = spramMap toMem
  | checkRegion 10              = romMap   toMem
  | checkRegion  2 && isDataMem = uartMap  toMem
  |                   isDataMem = ledMap   toMem
  | otherwise                   = Rom 0
  where
    isDataMem = memAccess toMem == DataMem
    checkRegion n = bitToBool $ lsb $ memAddress toMem `shiftR` n

busMapOut :: Bus -> BitVector 32 -> BitVector 32 -> BitVector 32 -> BitVector 32
busMapOut busOut fromSpram fromBios fromUart = case busOut of
  (Spram _ _ _ _) -> fromSpram
  (Rom _)       -> fromBios
  _             -> fromUart


