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

uartM 
  :: BitVector 32 -- ^ uart memory address
  -> Maybe ToMem  -- ^ memory access
  -> RWS Bit Bit Uart (First (BitVector 32)) -- ^ uart monadic action
uartM toMemM = do
  forM_ 