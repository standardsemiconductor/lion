{-|
Module : Bus
Description : Lion SoC Bus and Memory Map
Copyright : (c) David Cox, 2021
License : BSD-3-Clause
Maintainer : standardsemiconductor@gmail.com
-}

module Bus where

import Clash.Prelude
import Lion.Core (ToMem(..))

data Bus = Rom -- ^ rom access
             (Unsigned 8) -- ^ rom word address
         | Led -- ^ LED access
           (BitVector 4) -- ^ LED IP Register Address
           (BitVector 8) -- ^ LED IP Register Write Data
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

romMap :: ToMem -> Maybe Bus
romMap = Just . Rom . wordAddr . getAddress
  where
    wordAddr :: BitVector 32 -> Unsigned 8
    wordAddr a = unpack $ slice d7 d0 $ a `shiftR` 2

ledMap :: ToMem -> Maybe Bus
ledMap = \case
  DataMem _ $(bitPattern "..11") (Just d) -> Just $ Led (slice d11 d8 d) (slice d7 d0 d)
  _ -> Nothing

busMapIn :: ToMem -> Maybe Bus
busMapIn toMem = case getAddress toMem of
  $(bitPattern ".....................1..........") -> romMap  toMem -- rom
  _ -> ledMap toMem

busMapOut :: Maybe Bus -> BitVector 32 -> BitVector 32
busMapOut busOut fromBios = case busOut of
  Just (Rom _) -> fromBios
  _ -> 0

-------------
-- Utility --
-------------

getAddress :: ToMem -> BitVector 32
getAddress = \case
  InstrMem a     -> a
  DataMem  a _ _ -> a
           