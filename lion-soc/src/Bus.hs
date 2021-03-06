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
data Bus = Bios -- ^ bios access 
             (Unsigned 8) -- ^ bios word address
         | Led -- ^ LED access 
             (BitVector 4) -- ^ LED IP Register Address
             (BitVector 8) -- ^ LED IP Register Write Data
         | Uart -- ^ UART access 
             (BitVector 3)         -- ^ UART mask
             (Maybe (BitVector 8)) -- ^ UART write value
             
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

-- | forget whether memory access is instruction or data.
forgetAccessType :: ToMem -> (BitVector 32, BitVector 4, Maybe (BitVector 32))
forgetAccessType = \case
  InstrMem addr        -> (addr, 0xF, Nothing)
  DataMem addr msk wrM -> (addr, msk, wrM)

mkBus :: ToMem -> Maybe Bus
mkBus = busMap . forgetAccessType

busMap :: (BitVector 32, BitVector 4, Maybe (BitVector 32)) -> Maybe Bus
busMap (addr, msk, wrM) = case addr of
  $(bitPattern "000000000000000000000000........") -> -- bios
    let wordAddr = unpack $ slice d7 d0 $ addr `shiftR` 2
    in Just $ Bios wordAddr
  $(bitPattern "000000000000000000000001000000..") -> case (msk, wrM) of -- led
    ( $(bitPattern "..11"), Just d ) -> Just $ Led (slice d11 d8 d) (slice d7 d0 d)
    _ -> Nothing
  $(bitPattern "000000000000000000000001000001..") -> -- uart
    Just $ Uart (slice d2 d0 msk) $ slice d7 d0 <$> wrM
--    ( $(bitPattern ".100"), Nothing) -> Just $ Uart UartStatus
--    ( $(bitPattern ".010"), Nothing) -> Just $ Uart UartRead
--    ( $(bitPattern ".001"), Just d)  -> Just $ Uart $ UartWrite $ slice d7 d0 d
  _ -> Nothing
