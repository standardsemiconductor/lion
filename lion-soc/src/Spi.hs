{-# LANGUAGE CPP #-}

{-|
Module      : Spi
Description : Lion SoC SPI peripheral
Copyright   : (c) David Cox, 2024
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com

Register Map
 | 31 ------- 24 | 23 ------------------------------ 16 | 15 --------- 0 |
 | SysBus Status | SysBus Read/Write OR SysBus Received | SysBus Command |

SysBus Status 0=Empty, otherwise Busy
SysBus Read/Write 0=Read, 1=Write
SysBus Command address=bits 15-8, data=7-0
-}

module Spi where

import Clash.Prelude
import Control.Monad.RWS
import Control.Lens hiding (Index)
import Data.Maybe ( fromMaybe, isJust )
import Data.Monoid.Generic
import qualified Ice40.Spi as S
import Ice40.IO
import Bus
#if __GLASGOW_HASKELL__ > 902
import Control.Monad (when)
import Data.Monoid (First(..))
#endif

data SpiIO = SpiIO ("biwo" ::: Bit)
                   ("bowi" ::: Bit)
                   ("wck"  ::: Bit)
                   ("cs"   ::: Bit)
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

data ToSysBus = ToSysBus
  { _sbAckO   :: Bool
  , _sbDatO   :: BitVector 8
  , _fromCore :: BusIn 'Spi
  }
makeLenses ''ToSysBus

data FromSysBus = FromSysBus
  { _sbRWI  :: First Bool
  , _sbStbI :: First Bool
  , _sbAdrI :: First (BitVector 8)
  , _sbDatI :: First (BitVector 8)
  , _toCore :: First (BitVector 32)
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
  deriving Semigroup via GenericSemigroup FromSysBus
  deriving Monoid via GenericMonoid FromSysBus
makeLenses ''FromSysBus

data SysBus = SysBus
  { _sbInstr :: Maybe (BitVector 8, Maybe (BitVector 8))
  , _sbRecv  :: BitVector 8
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''SysBus

mkSysBus :: SysBus
mkSysBus = SysBus Nothing 0

sysBusM :: RWS ToSysBus FromSysBus SysBus ()
sysBusM = do
  instr <- use sbInstr

  -- output sysbus signals
  scribe sbRWI  $ First $ isJust . snd <$> instr
  scribe sbStbI $ First $ True <$ instr
  scribe sbAdrI $ First $ fst <$> instr
  scribe sbDatI $ First $ snd =<< instr

  -- when ack received, set instr to Nothing indicating done
  -- and store sbDatO in sbRecv
  isAck <- view sbAckO
  when isAck $ do
    sbInstr .= Nothing
    sbRecv <~ view sbDatO

  -- handle memory requests
  view fromCore >>= \case
    ToSpi $(bitPattern "1111") (Just wr) -> -- command
      case instr of
        Just _  -> return () -- busy, ignore command
        Nothing ->           -- idle, execute command
          let isWrite = bitToBool $ wr!(16 :: Index 32)
              adri = slice d15 d8 wr
              dati = slice d7  d0 wr
          in sbInstr ?= if isWrite
               then (adri, Just dati)
               else (adri, Nothing)
    ToSpi $(bitPattern "0100") Nothing -> -- read received
      scribe toCore . First . Just =<< uses sbRecv ((`shiftL` 16).zeroExtend)
    _ -> -- read status, default instruction
      scribe toCore $ First $ Just $ if isJust instr
        then 0x01000000
        else 0x00000000

sysBus
  :: HiddenClockResetEnable dom
  => Signal dom ToSysBus
  -> Signal dom FromSysBus
sysBus = mealy sysBusMealy mkSysBus
  where
    sysBusMealy s i = (s', o)
      where
        ((), s', o) = runRWS sysBusM i s

{-# NOINLINE spi #-}
spi
  :: HiddenClockResetEnable dom
  => Signal dom (BusIn 'Spi)
  -> Unbundled dom (SpiIO, BusOut 'Spi)
spi toSpi = (spiIO, fromSpi)
  where
    fromSpi = fmap (FromSpi . fromMaybe 0) $ register Nothing $ getFirst . _toCore <$> fromSysBus
    fromSysBus = sysBus $ ToSysBus <$> sbacko
                                   <*> sbdato
                                   <*> toSpi

    spiIO = SpiIO <$> biwo <*> bowi <*> wck <*> cs
    (biwo, bi)   = biwoIO woe    wo
    (bowi, wi)   = bowiIO boe    bo
    (wck,  wcki) = wckIO  wckoe  wcko
    (cs, wcsni)  = csIO   bcsnoe bcsno

    rwi  = fromMaybe False . getFirst . _sbRWI  <$> fromSysBus
    stbi = fromMaybe False . getFirst . _sbStbI <$> fromSysBus
    adri = fromMaybe 0 . getFirst . _sbAdrI <$> fromSysBus
    dati = fromMaybe 0 . getFirst . _sbDatI <$> fromSysBus

    (sbdato, sbacko, _, _, wo, woe, bo, boe, wcko, wckoe, bcsno, bcsnoe)
      = S.spi "0b0000"
              rwi
              stbi
              adri
              dati
              bi
              wi
              wcki
              wcsni

{-# NOINLINE biwoIO #-}
biwoIO
  :: HiddenClock dom
  => Signal dom Bit
  -> Signal dom Bit
  -> Unbundled dom (Bit, Bit)
biwoIO woe wo = (biwo, bi)
  where
    (biwo, bi, _) = io PinInput
                       PinNoOutput
                       0 -- pullUp
                       0 -- negTrigger
                       SBLVCMOS
                       0
                       0
                       hasClock
                       hasClock
                       woe
                       wo
                       0

{-# NOINLINE bowiIO #-}
bowiIO
  :: HiddenClock dom
  => Signal dom Bit
  -> Signal dom Bit
  -> Unbundled dom (Bit, Bit)
bowiIO boe bo = (bowi, wi)
  where
    (bowi, wi, _) = io PinInput
                       PinOutputTristate
                       0 -- pullup
                       0 -- negTrigger
                       SBLVCMOS
                       0 -- latchInputValue
                       0 -- clock enable
                       hasClock
                       hasClock
                       boe -- output enable
                       bo  -- dOut0
                       0

{-# NOINLINE wckIO #-}
wckIO
  :: HiddenClock dom
  => Signal dom Bit
  -> Signal dom Bit
  -> Unbundled dom (Bit, Bit)
wckIO wckoe wcko = (wck, wcki)
  where
    (wck, wcki, _) = io PinInput
                        PinOutputTristate
                        1 -- pullUp
                        0 -- negTrigger
                        SBLVCMOS
                        0 -- latchInputValue
                        0 -- clock enable
                        hasClock
                        hasClock
                        wckoe
                        wcko
                        0

{-# NOINLINE csIO #-}
csIO
  :: HiddenClock dom
  => Signal dom (BitVector 4)
  -> Signal dom (BitVector 4)
  -> Unbundled dom (Bit, Bit)
csIO bcsnoe bcsno = (cs, wcsni)
  where
    (cs, wcsni, _) = io PinInput
                        PinOutputTristate
                        1 -- pullUp
                        0 -- negTrigger
                        SBLVCMOS
                        0 -- latch input value
                        0 -- clock enable
                        hasClock
                        hasClock
                        ((! (3 :: Index 4)) <$> bcsnoe) -- output enable
                        ((! (3 :: Index 4)) <$> bcsno)  -- dOut0
                        0 -- dOut1


