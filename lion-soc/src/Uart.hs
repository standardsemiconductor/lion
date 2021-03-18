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
import Data.Maybe ( isJust, fromMaybe )
import Data.Monoid.Generic
-- import Ice40.IO

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
  { _fromBus :: B.Bus
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

fromStatus :: KnownNat n => Status -> BitVector (n + 1)
fromStatus = boolToBV . (== Full)

-- | Uart state
data Uart = Uart
  { _bus'     :: B.Bus -- ^ delayed bus
  , -- transmitter state
    _txIdx    :: Index 10             -- ^ buffer bit index
  , _txBaud   :: Index 625            -- ^ baud rate counter (19200 @ 12Mhz)
  , _txBuffer :: Maybe (BitVector 10) -- ^ transmitter data buffer
    -- receiver state
  , _rxFsm    :: RxFsm       -- ^ receiver fsm
  , _rxIdx    :: Index 8     -- ^ buffer index
  , _rxBaud   :: Index 625   -- ^ baud rate counter (19200 @ 12Mhz)
  , _rxStatus :: Status      -- ^ receiver status
  , _rxBuffer :: Vec 8 Bit -- ^ receiver data buffer
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''Uart 

-- | Construct a Uart
mkUart :: Uart
mkUart = Uart
  { _bus' = B.Rom 0 -- default bus
  , -- transmitter state
    _txIdx    = 0
  , _txBaud   = 0
  , _txBuffer = Nothing
    -- receiver state
  , _rxFsm    = RxIdle
  , _rxIdx    = 0
  , _rxBaud   = 0
  , _rxStatus = Empty
  , _rxBuffer = repeat 0
  }

-- | Uart output
data FromUart = FromUart
  { _tx     :: First Bit
  , _toCore :: First (BitVector 32)
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
  deriving Semigroup via GenericSemigroup FromUart
  deriving Monoid via GenericMonoid FromUart
makeLenses ''FromUart

-- | transmit 
transmit :: RWS ToUart FromUart Uart ()
transmit = use bus' >>= \case
  B.Uart $(bitPattern "001") (Just wr) -> do -- write send byte
    txIdx    .= 0
    txBaud   .= 0
    txBuffer ?= frame wr
  _ -> do
    bufferM <- use txBuffer
    scribe tx $ First $ lsb <$> bufferM
    forM_ bufferM $ const $ do
      ctr <- txBaud <<%= increment
      when (ctr == maxBound) $ do
        txBuffer %= fmap (`shiftR` 1)
        idx <- txIdx <<%= increment
        when (idx == maxBound) $ txBuffer .= Nothing
  where
    frame b = (1 :: BitVector 1) ++# b ++# (0 :: BitVector 1)

receive :: RWS ToUart FromUart Uart ()
receive = do
  rxIn <- view rx
  use bus' >>= \case
    B.Uart $(bitPattern "010") Nothing -> do -- read recv byte
      scribe toCore . First . Just =<< uses rxBuffer ((`shiftL` 8).zeroExtend.v2bv)
      rxReset
    _ -> use rxFsm >>= \case
      RxIdle -> 
        when (rxIn == low) $ do
          rxBaud %= increment
          rxFsm  %= increment
      RxStart -> do
        ctr <- rxBaud <<%= increment
        let baudHalf = maxBound `shiftR` 1
        when (ctr == baudHalf) $ do 
          rxBaud .= 0
          if rxIn == low
            then rxFsm %= increment
            else rxReset
      RxRecv -> do
        ctr <- rxBaud <<%= increment
        when (ctr == maxBound) $ do
          idx <- rxIdx <<%= increment
          rxBuffer %= (rxIn +>>)
          when (idx == maxBound) $ 
            rxFsm %= increment
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
  use bus' >>= \case
    B.Uart $(bitPattern "100") Nothing -> do  -- read status byte
      rxS <- uses rxStatus fromStatus
      txS <- uses txBuffer $ boolToBV . isJust 
      let status = (rxS `shiftL` 1) .|. txS
      scribe toCore $ First $ Just $ status `shiftL` 16
    _ -> return ()
  transmit
  receive
  bus' <~ view fromBus

uartMealy :: Uart -> ToUart -> (Uart, FromUart)
uartMealy s i = (s', o)
  where 
    (_, s', o) = runRWS uartM i s

uart
  :: HiddenClockResetEnable dom 
  => Signal dom Bit                    -- ^ uart rx
  -> Signal dom B.Bus                  -- ^ soc bus 
  -> Unbundled dom (Bit, BitVector 32) -- ^ (uart tx, toCore)
uart rxIn mem = (txOut, uartOut)
  where
    uartOut  = fromMaybe 0 . getFirst . _toCore  <$> fromUart
    txOut    = fromMaybe 1 . getFirst . _tx <$> fromUart
    fromUart = mealy uartMealy mkUart $ ToUart <$> mem <*> rxIn

-------------
-- Utility --
-------------
increment :: (Eq a, Enum a, Bounded a) => a -> a
increment a | a == maxBound = minBound
            | otherwise     = succ a
