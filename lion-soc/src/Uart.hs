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


-- | Receiver status
data Status = Empty | Full
  deriving stock (Generic, Show, Eq, Enum)
  deriving anyclass NFDataX

fromStatus :: Status -> BitVector n
fromStatus Empty = 0
fromStatus Full  = 1

-- | Uart state
data Uart = Uart
  { -- transmitter state
  , _txIdx    :: Index 10             -- ^ buffer bit index
  , _txBaud   :: Index 625            -- ^ baud rate counter (19200)
  , _txBuffer :: Maybe (BitVector 10) -- ^ transmitter data buffer
    -- receiver state
  , _rxFsm    :: RxFsm       -- ^ receiver fsm
  , _rxStatus :: RxStatus    -- ^ receiver status
  , _rxBuffer :: BitVector 8 -- ^ receiver data buffer
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''Uart 

-- | Construct a Uart
mkUart :: Uart
mkUart = Uart
  { -- transmitter state
  , _txIdx    = 0
  , _txBaud   = 0
  , _txBuffer = Nothing
    -- receiver state
  , _
  }

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
transmit = do
  bufferM <- use txBuffer
  forM_ bufferM $ \buf -> do
    scribe tx . First . Just =<< uses txIdx (buf!)
    ctr <- txBaud <<%= increment
    when (ctr == maxBound) $ do
      idx <- txIdx <<%= increment
      when (idx == maxBound) txReset
          
txReset :: MonadState Uart m => m ()
txReset = do
  txIdx    .= 0
  txBaud   .= 0
  txBuffer .= Nothing

receive :: RWS Bit FromUart Uart ()
receive = use rxFsm >>= \case
  RxIdle -> do
    rxLow <- views rx (== low)
    when rxLow $ do
      rxCtr %= increment
      rxFsm %= increment
  RxStart -> do
    rxLow    <- views rx (== low)
    ctr      <- use rxCtr
    baudHalf <- uses rxBaud (`shiftR` 1)
    if ctr == baudHalf
      then do 
        rxCtr .= 0
        if rxLow
          then rxFsm %= increment
          else rxReset
      else rxCtr %= increment
  RxRecv -> do
    ctr <- rxBaud <<%= increment
    when (ctr == maxBound) $ do
      rxBit <- view rx
      idx   <- rxIdx  <<%= increment
      rxBuf %= replaceBit (7 - idx) rxBit
      when (idx == maxBound) $ rxFsm %= increment
  RxStop -> do
    ctr <- rxBaud <<%= increment
    when (ctr == maxBound) $ do
      rxStatus .= Full
      rxFsm %= increment

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
      Bus _ $(bitPattern ".010") Nothing -> do -- read recv byte
        scribe fromUart . First . Just =<< use rxBuffer
        rxReset
      Bus _ $(bitPattern ".001") (Just wr) -> do -- write send byte
        txReset
        txBuffer ?= (1 :: BitVector 1) ++# slice d7 d0 wr
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

-------------
-- Utility --
-------------

increment :: (Eq a, Enum a, Bounded a) => a -> a
increment a | a == maxBound = minBound
            | otherwise     = succ a
