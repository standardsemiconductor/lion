{-|
Module      : Uart
Description : Lion Soc Uart Peripheral
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com
-}

module Uart where

import Clash.Prelude

-- | uart register
--   31 - 24 : 23 - 16 : 15 - 8 : 7 - 0
--     resvd : status  :  recv  : send
--
--   bits 31 - 24: reserved
--   bits 23 - 16: status byte, bit 17 - 0 = receiver empty, 1 = receiver full
--                              bit 16 - 0 = transmitter idle, 1 = transmitter busy
--                              status byte is read only
--   bits 15 - 8 : receiver buffer    -- read only -- reading this byte will reset the receiver
--   bits 7  - 0 : transmitter buffer -- write only -- writing this byte will reset the transmitter

-- | Transmitter finite-state machine
data TxFsm = TxStart
           | TxSend
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

-- | Receiver finite-state machine
data RxFsm = RxIdle
           | RxStart
           | RxRecv
           | RxStop
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

data Uart = Uart
  { _txFsm :: TxFsm
  , _rxFsm :: RxFsm
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''Uart 

newtype Tx = Tx { unTx :: Bit }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

instance Semigroup Tx where
  mappend = (.&.)

instance Monoid Tx where
  mempty = 1

data FromUart = FromUart
  { _tx       :: Tx
  , _fromUart :: First (BitVector 32)
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
  deriving Semigroup via GenericSemigroup FromUart
  deriving Monoid via GenericMonoid FromUart
makeLenses ''FromUart

uartM 
  :: BitVector 32 -- ^ uart memory address
  -> Maybe Bus  -- ^ memory access
  -> RWS Bit FromUart Uart () -- ^ uart monadic action
uartM busM = do
  transmit
  receive
  forM_ busM $ \bus -> _

uart
  :: HiddenClockResetEnable dom 
  => BitVector 32         -- ^ uart peripheral address
  -> Signal dom Maybe Bus -- ^ soc bus
  -> Signal dom Bit       -- ^ uart rx
  -> Unbundled dom (Bit, First (BitVector 32)) -- ^ (uart tx, toCore)
uart pAddr bus rx = (tx, toCore)
  where
    uartMealy s i = let = runRWS (uartM pAddr bus) rx mkUart
                    in (s', o)
  