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
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

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
  | checkRegion 10              = romMap  toMem
  | checkRegion  2 && isDataMem = uartMap toMem
  |                   isDataMem = ledMap  toMem
  | otherwise                   = Rom 0
  where
    isDataMem = memAccess toMem == DataMem
    checkRegion n = bitToBool $ lsb $ memAddress toMem `shiftR` n

busMapOut :: Bus -> BitVector 32 -> BitVector 32 -> BitVector 32
busMapOut busOut fromBios fromUart = case busOut of
  (Rom _) -> fromBios
  _       -> fromUart

