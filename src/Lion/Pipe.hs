{-|
Module      : Lion.Pipe
Description : RISC-V 5-stage pipeline
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com
-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Lion.Pipe where

import Clash.Prelude
import Control.Lens hiding ( (:<), (:>), imap, indices, op )
import Control.Monad.RWS
import Data.Function ( on )
import Data.Maybe ( isJust, fromMaybe )
import Data.Monoid.Generic
import Lion.Instruction
import Lion.Rvfi
import Lion.Util.Clash

-- | Pipeline configuration
data PipeConfig (startPC :: Nat) = PipeConfig
  deriving stock (Generic, Show, Eq)

-- | Default pipeline configuration
-- 
-- `startPC` = 0
defaultPipeConfig :: PipeConfig 0
defaultPipeConfig = PipeConfig

-- | Pipeline inputs
data ToPipe xl = ToPipe
  { _fromRs1 :: BitVector xl
  , _fromRs2 :: BitVector xl
  , _fromAlu :: BitVector xl
  , _fromMem :: BitVector xl
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''ToPipe

-- | Memory access - Lion has a shared instruction/memory bus
data MemoryAccess = InstrMem -- ^ instruction access
                  | DataMem  -- ^ data access
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

-- | Memory bus
data ToMem xl = ToMem
  { memAccess   :: MemoryAccess         -- ^ memory access type
  , memAddress  :: BitVector xl         -- ^ memory address
  , memByteMask :: BitVector (Div xl 8) -- ^ memory byte mask
  , memWrite    :: Maybe (BitVector xl) -- ^ read=Nothing write=Just wr
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

-- | Construct instruction memory access
instrMem 
  :: KnownNat xl
  => BitVector xl -- ^ instruction address
  -> ToMem xl
instrMem addr = ToMem
  { memAccess   = InstrMem
  , memAddress  = addr
  , memByteMask = 0xF
  , memWrite    = Nothing
  }

-- | Construct data memory access
dataMem 
  :: BitVector xl         -- ^ memory address
  -> BitVector (Div xl 8) -- ^ byte mask
  -> Maybe (BitVector xl) -- ^ write
  -> ToMem xl
dataMem addr mask wrM = ToMem
  { memAccess   = DataMem
  , memAddress  = addr
  , memByteMask = mask
  , memWrite    = wrM
  }

-- | Pipeline outputs
data FromPipe xl = FromPipe
  { _toMem       :: First (ToMem xl)
  , _toRs1Addr   :: First (Unsigned 5)
  , _toRs2Addr   :: First (Unsigned 5)
  , _toRd        :: First (Unsigned 5, BitVector xl)
  , _toAluOp     :: First Op
  , _toAluInput1 :: First (BitVector xl)
  , _toAluInput2 :: First (BitVector xl)
  , _toRvfi      :: First (Rvfi xl)
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
  deriving Semigroup via GenericSemigroup (FromPipe xl)
  deriving Monoid via GenericMonoid (FromPipe xl)
makeLenses ''FromPipe

data Control xl = Control
  { _firstCycle  :: Bool                             -- ^ First cycle True, then always False
  , _exBranching :: Maybe (BitVector xl)             -- ^ execute stage branch/jump
  , _meBranching :: Bool                             -- ^ memory stage branch/jump
  , _deLoad      :: Bool                             -- ^ decode stage load
  , _exLoad      :: Bool                             -- ^ execute stage load
  , _meMemory    :: Bool                             -- ^ memory stage load/store
  , _wbMemory    :: Bool                             -- ^ writeback stage load/store
  , _meRegFwd    :: Maybe (Unsigned 5, BitVector xl) -- ^ memory stage register forwarding
  , _wbRegFwd    :: Maybe (Unsigned 5, BitVector xl) -- ^ writeback stage register forwading
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''Control

mkControl :: Control xl
mkControl = Control 
  { _firstCycle  = True   
  , _exBranching = Nothing
  , _meBranching = False
  , _deLoad      = False
  , _exLoad      = False
  , _meMemory    = False
  , _wbMemory    = False
  , _meRegFwd    = Nothing
  , _wbRegFwd    = Nothing
  }

data Pipe xl = Pipe
  { _fetchPC :: BitVector xl

  -- decode stage
  , _dePC    :: BitVector xl

  -- execute stage
  , _exIR    :: Maybe (ExInstr xl)
  , _exPC    :: BitVector xl
  , _exRs1   :: Unsigned 5
  , _exRs2   :: Unsigned 5
  , _exRvfi  :: Rvfi xl

  -- memory stage
  , _meIR    :: Maybe (MeInstr xl)
  , _meRvfi  :: Rvfi xl

  -- writeback stage
  , _wbIR    :: Maybe (WbInstr xl)
  , _wbNRet  :: BitVector 64
  , _wbRvfi  :: Rvfi xl

  -- pipeline control
  , _control :: Control xl
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''Pipe

mkPipe 
  :: forall startPC xl
   . KnownNat xl
  => (KnownNat startPC, startPC + 1 <= 2^xl)
  => PipeConfig (startPC :: Nat) 
  -> Pipe xl
mkPipe _ = Pipe
  { _fetchPC = natToNum @startPC

  -- decode stage 
  , _dePC    = 0
  
  -- execute stage
  , _exIR    = Nothing
  , _exPC    = 0
  , _exRs1   = 0
  , _exRs2   = 0
  , _exRvfi  = mkRvfi

  -- memory stage
  , _meIR    = Nothing
  , _meRvfi  = mkRvfi
 
  -- writeback stage
  , _wbIR    = Nothing
  , _wbNRet  = 0
  , _wbRvfi  = mkRvfi
  
  -- pipeline control
  , _control = mkControl
  }

-- | 5-Stage RISC-V pipeline
pipe 
  :: HiddenClockResetEnable dom
  => (KnownNat xl, 1 <= Div xl 8)
  => (KnownNat startPC, startPC + 1 <= 2^xl)
  => PipeConfig (startPC :: Nat)
  -> Signal dom (ToPipe xl)
  -> Signal dom (FromPipe xl)
pipe config = mealy pipeMealy (mkPipe config)
  where
    pipeMealy s i = let ((), s', o) = runRWS pipeM i s
                    in (s', o) 

-- | Monadic pipeline
pipeM
  :: (KnownNat xl, 1 <= Div xl 8)
  => RWS (ToPipe xl) (FromPipe xl) (Pipe xl) ()
pipeM = do
  writeback
  memory
  execute
  decode
  fetch
  control .= mkControl{ _firstCycle = False } -- reset control

-- | Writeback stage
writeback
  :: KnownNat xl
  => RWS (ToPipe xl) (FromPipe xl) (Pipe xl) ()
writeback = withInstr wbIR $ \instr -> do
  wbRvfi.rvfiValid .= True
  wbRvfi.rvfiOrder <~ wbNRet <<+= 1
  case instr of
    WbRegWr rdAddr wr opWidth -> do
      wbRvfi.rvfiRdAddr .= rdAddr
      rdData <- wbRvfi.rvfiRdWData <.= guardZero rdAddr wr
      scribe toRd . First =<< control.wbRegFwd <.= Just (rdAddr, shorten opWidth rdData)
    WbLoad op rdAddr mask -> do
      control.wbMemory .= True
      wbRvfi.rvfiRdAddr .= rdAddr
      mem <- wbRvfi.rvfiMemRData <<~ view fromMem
      let mem8 = sliceX d8 mask mem
          mem16 = sliceX d16 mask mem
          mem32 = sliceX d32 mask mem
          wr = case op of
            Lb  -> signResize mem8
            Lh  -> signResize mem16
            Lw  -> signResize mem32
            Ld  -> mem
            Lbu -> zeroResize mem8
            Lhu -> zeroResize mem16
            Lwu -> zeroResize mem32
      rdData <- wbRvfi.rvfiRdWData <.= guardZero rdAddr wr
      scribe toRd . First =<< control.wbRegFwd <.= Just (rdAddr, rdData)
    WbStore -> control.wbMemory .= True
    WbNop -> return ()
  scribe toRvfi . First . Just =<< use wbRvfi
  where
    guardZero 0 = const 0
    guardZero _ = id

-- | Memory stage
memory :: RWS (ToPipe xl) (FromPipe xl) (Pipe xl) ()
memory = do
  wbIR   .= Nothing
  wbRvfi <~ use meRvfi
  withInstr meIR $ \case
    MeNop -> wbIR ?= WbNop
    MeRegWr rd opWidth -> do
      wr <- view fromAlu
      control.meRegFwd ?= (rd, wr)
      wbIR ?= WbRegWr rd wr opWidth
    MeJump rd pc4 -> do
      control.meBranching .= True
      control.meRegFwd ?= (rd, pc4)
      wbIR ?= WbRegWr rd pc4 FullWidth
    MeBranch -> do
      control.meBranching .= True
      wbIR ?= WbNop
    MeStore addr mask value -> do
      control.meMemory .= True
      scribe toMem $ First $ Just $ dataMem addr mask $ Just value
      wbRvfi.rvfiMemAddr  .= addr
      wbRvfi.rvfiMemWMask .= mask
      wbRvfi.rvfiMemWData .= value
      wbIR ?= WbStore
    MeLoad op rdAddr addr mask -> do
      control.meMemory .= True
      scribe toMem $ First $ Just $ dataMem addr mask Nothing
      wbRvfi.rvfiMemAddr  .= addr
      wbRvfi.rvfiMemRMask .= mask
      wbIR ?= WbLoad op rdAddr mask

-- | Execute stage
execute :: forall xl
  .  (KnownNat xl, 1 <= Div xl 8)
  => RWS (ToPipe xl) (FromPipe xl) (Pipe xl) ()
execute = do
  meIR .= Nothing
  meRvfi <~ use exRvfi
  pc  <- meRvfi.rvfiPcRData <<~ use exPC
  pc4 <- meRvfi.rvfiPcWData <.= pc + 4
  rs1Data <- meRvfi.rvfiRs1Data <<~ regFwd exRs1 fromRs1 (control.meRegFwd) (control.wbRegFwd)
  rs2Data <- meRvfi.rvfiRs2Data <<~ regFwd exRs2 fromRs2 (control.meRegFwd) (control.wbRegFwd)
  withInstr exIR $ \case
    Ex op rd imm -> do
      scribeAlu Add imm $ case op of
        Lui   -> 0
        Auipc -> pc
      meIR ?= MeRegWr rd FullWidth
    ExJump jump rd imm -> do
      npc <- meRvfi.rvfiPcWData <<~ control.exBranching <?= case jump of
        Jal  -> pc + imm
        Jalr -> clearBit (rs1Data + imm) 0
      meRvfi.rvfiTrap ||= isMisaligned d32 npc
      meIR ?= MeJump rd pc4
    ExBranch op imm ->
      if branch op rs1Data rs2Data
        then do
          branchPC <- meRvfi.rvfiPcWData <<~ control.exBranching <?= pc + imm
          meRvfi.rvfiTrap ||= isMisaligned d32 branchPC
          meIR ?= MeBranch
        else do
          meRvfi.rvfiTrap ||= isMisaligned d32 pc4
          meIR ?= MeNop
    ExStore op imm ->
      let addr = rs1Data + imm            -- unaligned
          addr' = addr .&. fromIntegral (natVal (SNat :: SNat (Div xl 8)) - 1) -- aligned
          helper :: _ => SNat yl -> _
          helper yl = do
            meRvfi.rvfiTrap ||= isMisaligned yl addr -- trap on boundary
            let wr :: BitVector xl
                wr = concatReplicateI $ resizeTo yl rs2Data
            meIR ?= MeStore addr' (byteMaskX yl addr) wr
      in case op of
        Sb -> helper d8
        Sh -> helper d16
        Sw -> helper d32
        Sd -> helper d64
    ExLoad op rdAddr imm -> do
      control.exLoad .= True
      let addr = rs1Data + imm            -- unaligned
          addr' = addr .&. fromIntegral (natVal (SNat :: SNat (Div xl 8)) - 1) -- aligned
          helper :: _ => SNat yl -> _
          helper yl = do
             meRvfi.rvfiTrap ||= isMisaligned yl addr -- trap on boundary
             meIR ?= MeLoad op rdAddr addr' (byteMaskX yl addr)
      if | op == Lb || op == Lbu -> helper d8
         | op == Lh || op == Lhu -> helper d16
         | op == Lw || op == Lwu -> helper d32
         | otherwise -> helper d64
    ExAlu op rd opWidth -> do
      (scribeAlu op `on` shorten opWidth) rs1Data rs2Data
      meIR ?= MeRegWr rd FullWidth
    ExAluImm op rd imm opWidth -> do
      scribeAlu op (shorten opWidth rs1Data) imm
      meIR ?= MeRegWr rd FullWidth
  where
    scribeAlu op in1 in2 = do
      scribe toAluOp     $ First $ Just op
      scribe toAluInput1 $ First $ Just in1
      scribe toAluInput2 $ First $ Just in2

    regFwd 
      :: MonadState s m 
      => MonadReader r m
      => Lens' s (Unsigned 5) 
      -> Lens' r (BitVector xl)
      -> Lens' s (Maybe (Unsigned 5, BitVector xl))
      -> Lens' s (Maybe (Unsigned 5, BitVector xl))
      -> m (BitVector xl)
    regFwd rsAddr rsData meFwd wbFwd = 
      guardZero rsAddr =<< fwd <$> use rsAddr <*> view rsData <*> use meFwd <*> use wbFwd
      where
        guardZero  -- register x0 always has value 0.
          :: MonadState s m 
          => Lens' s (Unsigned 5) 
          -> BitVector xl 
          -> m (BitVector xl)
        guardZero addr value = do
          isZero <- uses addr (== 0)
          return $ if isZero
             then 0
             else value

-- | Decode stage
decode :: KnownNat xl => RWS (ToPipe xl) (FromPipe xl) (Pipe xl) ()
decode = do
  exIR .= Nothing
  exRvfi .= mkRvfi
  exPC <~ use dePC
  mem <- exRvfi.rvfiInsn <<~ (resize <$> view fromMem)
  scribe toRs1Addr . First . Just =<< exRvfi.rvfiRs1Addr <<~ exRs1 <.= sliceRs1 mem
  scribe toRs2Addr . First . Just =<< exRvfi.rvfiRs2Addr <<~ exRs2 <.= sliceRs2 mem
  isFirstCycle  <- use $ control.firstCycle -- first memory output undefined
  isMeBranching <- use $ control.meBranching
  isWbMemory    <- use $ control.wbMemory
  isExLoad      <- use $ control.exLoad
  isExBranching <- uses (control.exBranching) isJust
  let bubble = isFirstCycle || isMeBranching || isWbMemory || isExLoad || isExBranching
  case parseInstr mem of
    Right instr -> unless bubble $ do
      exIR ?= instr
      control.deLoad .= case instr of
        ExLoad{} -> True
        _        -> False
    Left IllegalInstruction -> do -- trap and instr=Nop (addi x0 x0 0)
      unless bubble $ exIR ?= ExAlu Add 0 FullWidth
      exRvfi.rvfiTrap .= True
        
-- | fetch instruction
fetch :: KnownNat xl => RWS (ToPipe xl) (FromPipe xl) (Pipe xl) ()
fetch = do
  scribe toMem . First . Just . instrMem =<< use fetchPC
  isMeMemory <- use $ control.meMemory
  isDeLoad   <- use $ control.deLoad
  use (control.exBranching) >>= \case
    Just npc -> fetchPC .= npc
    Nothing  -> unless (isMeMemory || isDeLoad) $ dePC <~ fetchPC <<+= 4

-------------
-- Utility --
-------------

-- | forward register writes
fwd 
  :: Unsigned 5 
  -> BitVector xl 
  -> Maybe (Unsigned 5, BitVector xl) -- ^ meRegFwd
  -> Maybe (Unsigned 5, BitVector xl) -- ^ wbRegFwd
  -> BitVector xl
fwd _    wr Nothing Nothing = wr
fwd addr wr Nothing (Just (wbAddr, wbWr))
  | addr == wbAddr = wbWr
  | otherwise      = wr
fwd addr wr (Just (meAddr, meWr)) Nothing
  | addr == meAddr = meWr
  | otherwise      = wr
fwd addr wr (Just (meAddr, meWr)) (Just (wbAddr, wbWr))
  | addr == meAddr = meWr
  | addr == wbAddr = wbWr
  | otherwise      = wr

-- | calcluate byte mask based on address
byteMaskX
  :: (KnownNat xl, 1 <= Div xl 8, 1 <= Div yl 8)
  => SNat yl -> BitVector xl -> BitVector (Div xl 8)
byteMaskX yl@SNat =
    shiftL (shiftL 1 (fromIntegral $ natVal $ divSNat yl d8) - 1) . unpack .
    (.&.) (fromIntegral $ complement $ natVal yl `div` 8 - 1) . zeroResize .
    (resizeTo =<< flogBaseSNat d2 . flip divSNat d8 . snatProxy)

sliceX :: forall xl yl
  .  (KnownNat xl, 1 <= yl)
  => SNat yl -> BitVector (Div xl 8) -> BitVector xl -> BitVector yl
sliceX yl@SNat mask = resizeTo yl . (pure 0 `fromMaybe` fold (<|>) (imap f mask' :< empty))
  where
    mask' :: Vec (Div xl yl) (BitVector (Div yl 8))
    mask' = unconcatBitVector# (resize mask)

    f k m = flip shiftR (fromIntegral (natVal yl)*fromEnum k) <$ guard (0 == complement m)

-- | check if memory address misaligned on word boundary
isMisaligned :: (Bits a, Num a, KnownNat xl) => SNat xl -> a -> Bool
isMisaligned xl a = 0 /= a .&. fromIntegral (natVal xl `div` 8 - 1)

-- | run monadic action when instruction is Just
withInstr :: MonadState s m => Lens' s (Maybe a) -> (a -> m ()) -> m ()
withInstr l k = use l >>= mapM_ k

-- | Hazards Note
--
-- Key:
-- J  = JAL
-- JR = JALR
-- O  = Bubble
-- S  = Store
-- *  = Stall
-- B  = Branch
--
-- Jump/Branch
-- +----+------+------+------+----+
-- | IF |  DE  |  EX  |  ME  | WB |
-- +====+======+======+======+====+
-- | 4  | ---- | ---- | ---- | -- |
-- +----+------+------+------+----+
-- | 8  | JR20 | ---- | ---- | -- |
-- +----+------+------+------+----+
-- | 12 |  O   | JR20 | ---- | -- |
-- +----+------+------+------+----+
-- | 20 |  O   |  O   | JR20 | -- |
-- +----+------+------+------+----+
--
-- Store
-- +-------+------+------+------+----+
-- |  IF   | DE   |  EX  |  ME  | WB |
-- +=======+======+======+======+====+
-- | 4     | ---- | ---- | ---- | -- |
-- +-------+------+------+------+----+
-- | 8     |  S   | ---- | ---- | -- |
-- +-------+------+------+------+----+
-- | 12    | J100 |  S   | ---- | -- |
-- +-------+------+------+------+----+
-- | *16*  |  O   | J100 |  S   | -- |
-- +-------+------+------+------+----+
-- |  100  |  O   |  O   | J100 | S  |
-- +-------+------+------+------+----+
--
-- Load
-- +------+------+------+------+----+
-- | IF   |  DE  |  EX  |  ME  | WB |
-- +======+======+======+======+====+
-- | 4    | ---- | ---- | ---- | -- |
-- +------+------+------+------+----+
-- | *8*  |  L   | ---- | ---- | -- |
-- +------+------+------+------+----+
-- | 8    |  O   |  L   | ---- | -- |
-- +------+------+------+------+----+
-- | *12* | B100 |  O   |  L   | -- |
-- +------+------+------+------+----+
-- |  12  |  O   | B100 |  O   | L  |
-- +------+------+------+------+----+
-- | 100  |  O   |  O   | B100 | O  |
-- +------+------+------+------+----+
