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
--                              bit 16 - 0 = transmitter empty (idle), 1 = transmitter full (busy)
--                              status byte is read only
--   bits 15 - 8 : receiver buffer    -- read only -- reading this byte will reset the receiver
--   bits 7  - 0 : transmitter buffer -- write only -- writing this byte will reset the transmitter

-- | Transmitter finite-state machine
data TxFsm = TxStart
           | TxSend
  deriving stock (Generic, Show, Eq, Enum, Bounded)
  deriving anyclass NFDataX

-- | Receiver finite-state machine
data RxFsm = RxIdle
           | RxStart
           | RxRecv
           | RxStop
  deriving stock (Generic, Show, Eq, Enum, Bounded)
  deriving anyclass NFDataX

-- | increment fsm
next :: (Eq a, Enum a, Bounded a) => a -> a
next s | s == maxBound = minBound
       | otherwise     = succ s

-- | Transmitter/Receiver status
data Status = Empty | Full
  deriving stock (Generic, Show, Eq, Enum)
  deriving anyclass NFDataX

fromStatus :: Status -> BitVector n
fromStatus Empty = 0
fromStatus Full  = 1

-- | Uart state
data Uart = Uart
  { -- transmitter state
    _txFsm    :: TxFsm        -- ^ transmitter fsm
  , _txStatus :: Status       -- ^ transmitter status
  , _txBuffer :: BitVector 10 -- ^ transmitter data buffer
    -- receiver state
  , _rxFsm    :: RxFsm       -- ^ receiver fsm
  , _rxStatus :: RxStatus    -- ^ receiver status
  , _rxBuffer :: BitVector 8 -- ^ receiver data buffer
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''Uart 

-- | Tx wire
newtype Tx = Tx { unTx :: Bit }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

instance Semigroup Tx where
  mappend = (.&.)

instance Monoid Tx where
  mempty = 1

-- | Uart output
data FromUart = FromUart
  { _tx       :: Tx
  , _fromUart :: First (BitVector 32)
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
  deriving Semigroup via GenericSemigroup FromUart
  deriving Monoid via GenericMonoid FromUart
makeLenses ''FromUart

-- | transmit 
transmit :: RWS Bit FromUart Uart ()
transmit = use 

receive :: RWS Bit FromUart Uart ()
receive = _

uartM 
  :: BitVector 32 -- ^ uart memory address
  -> Maybe Bus  -- ^ memory access
  -> RWS Bit FromUart Uart () -- ^ uart monadic action
uartM uartAddr busM = do
  transmit
  receive
  forM_ busM $ \bus ->
    when (busAddr bus == uartAddr) $ case bus of
      Bus _ $(bitPattern ".100") Nothing -> do  -- read status byte
        rxS <- uses rxStatus fromStatus
        txS <- uses txStatus fromStatus
        let status = (rxS `shiftL` 1) .|. txS
        scribe fromUart $ First $ Just $ status `shiftL` 16
      Bus _ $(bitPattern ".010") Nothing   -> _ -- read recv byte
      Bus _ $(bitPattern ".001") (Just wr) -> _ -- write send byte
      _ -> return ()

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

