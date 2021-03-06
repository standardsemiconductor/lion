{-|
Module      : Uart
Description : Lion Soc Uart Peripheral
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com
-}

module Uart where

import Clash.Prelude
import qualified Bus as B
import Control.Lens hiding (Index, Empty)
import Control.Monad.RWS
import Data.Maybe ( isJust )
import Data.Monoid.Generic

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

data ToUart = ToUart
  { _fromBus :: Maybe B.Bus
  , _rx      :: Bit
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''ToUart
  
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

fromStatus :: KnownNat n => Status -> BitVector n
fromStatus Empty = 0
fromStatus Full  = 1

-- | Uart state
data Uart = Uart
  { -- transmitter state
    _txIdx    :: Index 10             -- ^ buffer bit index
  , _txBaud   :: Index 625            -- ^ baud rate counter (19200 @ 12Mhz)
  , _txBuffer :: Maybe (BitVector 10) -- ^ transmitter data buffer
    -- receiver state
  , _rxFsm    :: RxFsm       -- ^ receiver fsm
  , _rxIdx    :: Index 8     -- ^ buffer index
  , _rxBaud   :: Index 625   -- ^ baud rate counter (19200 @ 12Mhz)
  , _rxStatus :: Status      -- ^ receiver status
  , _rxBuffer :: BitVector 8 -- ^ receiver data buffer
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''Uart 

-- | Construct a Uart
mkUart :: Uart
mkUart = Uart
  { -- transmitter state
    _txIdx    = 0
  , _txBaud   = 0
  , _txBuffer = Nothing
    -- receiver state
  , _rxFsm    = RxIdle
  , _rxIdx    = 0
  , _rxBaud   = 0
  , _rxStatus = Empty
  , _rxBuffer = 0
  }

-- | Tx wire
newtype Tx = Tx { unTx :: Bit }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

instance Semigroup Tx where
  Tx a <> Tx b = Tx $ a .&. b

instance Monoid Tx where
  mempty = Tx 1

-- | Uart output
data FromUart = FromUart
  { _tx     :: Tx
  , _toCore :: First (BitVector 32)
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
  deriving Semigroup via GenericSemigroup FromUart
  deriving Monoid via GenericMonoid FromUart
makeLenses ''FromUart

-- | transmit 
transmit :: RWS ToUart FromUart Uart ()
transmit = do
  bufferM <- use txBuffer
  forM_ bufferM $ \buf -> do
    scribe tx . Tx =<< uses txIdx (buf!)
    ctr <- txBaud <<%= increment
    when (ctr == maxBound) $ do
      idx <- txIdx <<%= increment
      when (idx == maxBound) txReset
          
txReset :: MonadState Uart m => m ()
txReset = do
  txIdx    .= 0
  txBaud   .= 0
  txBuffer .= Nothing

receive :: RWS ToUart FromUart Uart ()
receive = use rxFsm >>= \case
  RxIdle -> do
    rxLow <- views rx (== low)
    when rxLow $ do
      rxBaud %= increment
      rxFsm  %= increment
  RxStart -> do
    rxLow <- views rx (== low)
    ctr   <- use rxBaud
    let baudHalf = maxBound `shiftR` 1
    if ctr == baudHalf
      then do 
        rxBaud .= 0
        if rxLow
          then rxFsm %= increment
          else rxReset
      else rxBaud %= increment
  RxRecv -> do
    ctr <- rxBaud <<%= increment
    when (ctr == maxBound) $ do
      rxBit <- view rx
      idx   <- rxIdx <<%= increment
      rxBuffer %= replaceBit (7 - idx) rxBit
      when (idx == maxBound) $ rxFsm %= increment
  RxStop -> do
    ctr <- rxBaud <<%= increment
    when (ctr == maxBound) $ do
      rxStatus .= Full
      rxFsm %= increment

rxReset :: MonadState Uart m => m ()
rxReset = do
  rxIdx    .= 0
  rxBaud   .= 0
  rxStatus .= Empty
  rxFsm    .= RxIdle

uartM :: RWS ToUart FromUart Uart () -- ^ uart monadic action
uartM = do
  transmit
  receive
  view fromBus >>= \case
    Just (B.Uart $(bitPattern "100") Nothing) -> do  -- read status byte
      rxS <- uses rxStatus fromStatus
      txS <- uses txBuffer $ boolToBV . isJust 
      let status = (rxS `shiftL` 1) .|. txS
      scribe toCore $ First $ Just $ status `shiftL` 16
    Just (B.Uart $(bitPattern "010") Nothing) -> do -- read recv byte
      scribe toCore . First . Just =<< uses rxBuffer ((`shiftL` 8).zeroExtend)
      rxReset
    Just (B.Uart $(bitPattern "001") (Just wr)) -> do -- write send byte
      txReset
      txBuffer ?= frame wr
    _ -> return ()
  where
    frame b = (1 :: BitVector 1) ++# b ++# (0 :: BitVector 1)

uartMealy :: Uart -> ToUart -> (Uart, FromUart)
uartMealy s i = let ((), s', o) = runRWS uartM i s
                in (s', o)

uart
  :: HiddenClockResetEnable dom 
  => Signal dom (Maybe B.Bus) -- ^ soc bus
  -> Signal dom Bit         -- ^ uart rx
  -> Unbundled dom (Bit, First (BitVector 32)) -- ^ (uart tx, toCore)
uart bus rxIn = (unTx . _tx <$> fromUart, uartOut)
  where
    uartOut = delay (First Nothing) $ _toCore <$> fromUart
    fromUart = mealy uartMealy mkUart $ ToUart <$> bus <*> rxIn

-------------
-- Utility --
-------------

increment :: (Eq a, Enum a, Bounded a) => a -> a
increment a | a == maxBound = minBound
            | otherwise     = succ a
