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
import Data.Bool ( bool )
import Data.Maybe ( isJust, fromMaybe )
import Data.Monoid.Generic
import Lion.Util.Clash

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

data ToUart xl = ToUart
  { _fromBus :: B.BusIn xl 'B.Uart
  , _rx      :: Bit
  }
makeLenses ''ToUart
  
-- | Receiver finite-state machine
data RxFsm = RxIdle
           | RxStart
           | RxRecv
           | RxStop
  deriving stock (Generic, Show, Eq, Enum, Bounded)
  deriving anyclass NFDataX

-- | Uart state
data Uart xl = Uart
  { -- transmitter state
    _txIdx    :: Index 10             -- ^ buffer bit index
  , _txBaud   :: Index 625            -- ^ baud rate counter (19200 @ 12Mhz)
  , _txBuffer :: Maybe (BitVector 10) -- ^ transmitter data buffer
    -- receiver state
  , _rxFsm    :: RxFsm                -- ^ receiver fsm
  , _rxIdx    :: Index 8              -- ^ buffer index
  , _rxBaud   :: Index 625            -- ^ baud rate counter (19200 @ 12Mhz)
  , _rxBuffer :: Vec 8 Bit            -- ^ receiver data buffer
  , _rxRecv   :: Maybe (BitVector xl) -- ^ store received bytes for reading
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''Uart 

-- | Construct a Uart
mkUart :: Uart xl
mkUart = Uart
  { -- transmitter state
    _txIdx    = 0
  , _txBaud   = 0
  , _txBuffer = Nothing
    -- receiver state
  , _rxFsm    = RxIdle
  , _rxIdx    = 0
  , _rxBaud   = 0
  , _rxBuffer = repeat 0
  , _rxRecv   = Nothing
  }

-- | Uart output
data FromUart xl = FromUart
  { _tx     :: First Bit
  , _toCore :: First (BitVector xl)
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
  deriving Semigroup via GenericSemigroup (FromUart xl)
  deriving Monoid via GenericMonoid (FromUart xl)
makeLenses ''FromUart

uartM :: KnownNat xl => RWS (ToUart xl) (FromUart xl) (Uart xl) () -- ^ uart monadic action
uartM = do
  -- transmit
  bufferM <- use txBuffer
  scribe tx $ First $ lsb <$> bufferM
  forM_ bufferM $ const $ do
    ctr <- txBaud <<%= increment
    when (ctr == maxBound) $ do
      txBuffer %= fmap (`shiftR` 1)
      idx <- txIdx <<%= increment
      when (idx == maxBound) $ txBuffer .= Nothing

  -- handle memory IO
  view fromBus >>= \case
    B.ToUart $(bitPattern ".1.") Nothing -> do -- read recv byte
      scribe toCore . First =<< use rxRecv
      rxRecv .= Nothing
    B.ToUart _ (Just wr) -> do -- write send byte
      txIdx    .= 0
      txBaud   .= 0
      txBuffer ?= frame wr
    _ -> return () 

  -- read status
  rxS <- uses rxRecv   $ bool 0 1 . isJust
  let txS = bool 0 1 $ isJust bufferM
      status = (rxS `shiftL` 1) .|. txS
  scribe toCore $ First $ Just $ status `shiftL` 16

  -- receive
  rxIn <- view rx
  use rxFsm >>= \case
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
          else rxFsm .= RxIdle
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
        rxRecv <~ uses rxBuffer (Just . (`shiftL` 8) . zeroResize . v2bv)
        rxFsm %= increment
  where
    frame b = (1 :: BitVector 1) ++# b ++# (0 :: BitVector 1)

uartMealy :: KnownNat xl => Uart xl -> ToUart xl -> (Uart xl, FromUart xl)
uartMealy s i = (s', o)
  where 
    (_, s', o) = runRWS uartM i s

uart
  :: (HiddenClockResetEnable dom, KnownNat xl)
  => Signal dom Bit                        -- ^ uart rx
  -> Signal dom (B.BusIn xl 'B.Uart)          -- ^ soc bus 
  -> Unbundled dom (Bit, B.BusOut xl 'B.Uart) -- ^ (uart tx, toCore)
uart rxIn bus = (txOut, B.FromUart <$> uartOut)
  where
    uartOut  = register 0 $ fromMaybe 0 . getFirst . _toCore  <$> fromUart
    txOut    = fromMaybe 1 . getFirst . _tx <$> fromUart
    fromUart = mealy uartMealy mkUart $ ToUart <$> bus <*> rxIn

-------------
-- Utility --
-------------
increment :: (Eq a, Enum a, Bounded a) => a -> a
increment a | a == maxBound = minBound
            | otherwise     = succ a
