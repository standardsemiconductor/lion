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
data Bus = Rom -- ^ rom access 
             (Unsigned 8) -- ^ rom word address
         | Led -- ^ LED access 
             (BitVector 4) -- ^ LED IP Register Address
             (BitVector 8) -- ^ LED IP Register Write Data
         | Uart -- ^ UART access 
             (BitVector 3)         -- ^ UART mask
             (Maybe (BitVector 8)) -- ^ UART write value             
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

romMap :: ToMem -> Maybe Bus
romMap = Just . Rom . wordAddr . memAddress
  where
    wordAddr :: BitVector 32 -> Unsigned 8
    wordAddr a = unpack $ slice d7 d0 $ a `shiftR` 2

uartMap :: ToMem -> Maybe Bus
uartMap = \case
  ToMem DataMem _ msk wrM -> Just $ Uart (slice d2 d0 msk) $ slice d7 d0 <$> wrM
  _ -> Nothing

ledMap :: ToMem -> Maybe Bus
ledMap = \case
  ToMem DataMem _ $(bitPattern "..11") (Just d) -> Just $ Led (slice d11 d8 d) (slice d7 d0 d)
  _ -> Nothing

busMapIn :: ToMem -> Maybe Bus
busMapIn toMem = case memAddress toMem of
  $(bitPattern ".....................1..........") -> romMap  toMem -- rom
  $(bitPattern ".............................1..") -> uartMap toMem -- uart
  _ -> ledMap toMem

busMapOut :: Maybe Bus -> BitVector 32 -> BitVector 32 -> BitVector 32
busMapOut busOut fromBios fromUart = case busOut of
  Just (Rom _) -> fromBios
  _            -> fromUart

