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
romMap = \case
  InstrMem a@($(bitPattern "000000000000000000000000........")) 
    -> Just $ Rom $ wordAddr a
  DataMem  a@($(bitPattern "000000000000000000000000........")) _ Nothing 
    -> Just $ Rom $ wordAddr a
  _ -> Nothing
  where
    wordAddr :: BitVector 32 -> Unsigned 8
    wordAddr addr = unpack $ slice d7 d0 $ addr `shiftR` 2

ledMap :: ToMem -> Maybe Bus
ledMap = \case
  DataMem $(bitPattern "000000000000000000000001000000..") $(bitPattern "..11") (Just d) ->
    Just $ Led (slice d11 d8 d) (slice d7 d0 d)
  _ -> Nothing

uartMap :: ToMem -> Maybe Bus
uartMap = \case
  DataMem $(bitPattern "000000000000000000000001000001..") msk wrM ->
    Just $ Uart (slice d2 d0 msk) $ slice d7 d0 <$> wrM
  _ -> Nothing

getAddress :: ToMem -> BitVector 32
getAddress = \case
  InstrMem a     -> a
  DataMem  a _ _ -> a

{-
isDataMem :: ToMem -> Bool
isDataMem = \case
  InstrMem _     -> False
  Datamem  _ _ _ -> True
-}

busMapIn :: ToMem -> Maybe Bus
busMapIn toMem = case getAddress toMem of
  $(bitPattern "000000000000000000000000........") -> romMap toMem -- rom
  $(bitPattern "000000000000000000000001000000..") -> ledMap toMem -- led
  $(bitPattern "000000000000000000000001000001..") -> uartMap toMem -- uart
  _ -> Nothing

busMapOut :: Maybe ToMem -> BitVector 32 -> BitVector 32 -> BitVector 32
busMapOut toMem fromBios fromUart = case getAddress <$> toMem of
  Just $(bitPattern "000000000000000000000000........") -> fromBios
  Just $(bitPattern "000000000000000000000001000001..") -> fromUart
  _ -> 0