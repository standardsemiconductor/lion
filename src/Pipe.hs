module Pipe where

import Clash.Prelude
import Control.Lens hiding ( op )
import Control.Monad.RWS
import Data.Monoid.Generic
import Instruction
import Rvfi

data ToPipe = ToPipe
  { _fromRs1 :: BitVector 32
  , _fromRs2 :: BitVector 32
  , _fromMem :: Maybe (BitVector 32)
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''ToPipe

data ToMem = InstrMem (BitVector 32)
           | DataMem (BitVector 32) (BitVector 4) (Maybe (BitVector 32))
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

data FromPipe = FromPipe
  { _toMem     :: First ToMem
  , _toRs1Addr :: First (Unsigned 5)
  , _toRs2Addr :: First (Unsigned 5)
  , _toRd      :: First (Unsigned 5, BitVector 32)
  , _toRvfi    :: First Rvfi
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
  deriving Semigroup via GenericSemigroup FromPipe
  deriving Monoid via GenericMonoid FromPipe
makeLenses ''FromPipe

data Control = Idle
             | Fetching
             | Branching (BitVector 32)
             | Memorizing
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

{-
data Control = Control
  { _branching :: Maybe (BitVector 32)
  , _fetchNext :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''Control
-}
mkControl :: Control
mkControl = Fetching

data Pipe = Pipe
  { _fetchPC  :: BitVector 32

  -- decode stage
  , _dePC     :: BitVector 32
  , _deNPC    :: BitVector 32

  -- execute stage
  , _exIR      :: Maybe ExInstr
  , _exPC      :: BitVector 32
  , _exRs1Zero :: Bool
  , _exRs2Zero :: Bool
  , _exRvfi    :: Rvfi

  -- memory stage
  , _meIR     :: Maybe MeInstr
  , _meRvfi   :: Rvfi

  -- writeback stage
  , _wbIR     :: Maybe WbInstr
  , _wbNRet   :: BitVector 64
  , _wbRvfi   :: Rvfi

  -- pipeline control
  , _control :: Control
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''Pipe

mkPipe :: Pipe
mkPipe = Pipe
  { _fetchPC  = 0  

  -- decode stage 
  , _dePC     = 0
  , _deNPC    = 0
  
  -- execute stage
  , _exIR      = Nothing
  , _exPC      = 0
  , _exRs1Zero = False
  , _exRs2Zero = False
  , _exRvfi    = mkRvfi

  -- memory stage
  , _meIR     = Nothing
  , _meRvfi   = mkRvfi
 
  -- writeback stage
  , _wbIR   = Nothing
  , _wbNRet = 0
  , _wbRvfi = mkRvfi
  
  -- pipeline control
  , _control = mkControl
  }

pipe 
  :: HiddenClockResetEnable dom
  => Signal dom ToPipe
  -> Signal dom FromPipe
pipe = mealy pipeMealy mkPipe
  where
    pipeMealy s i = let ((), s', o) = runRWS pipeM i s
                    in (s', o) 

pipeM :: RWS ToPipe FromPipe Pipe ()
pipeM = do
  writeback
  memory
  execute
  decode
  fetch

writeback :: RWS ToPipe FromPipe Pipe ()
writeback = use wbIR >>= \instrM ->
  forM_ instrM $ \instr -> do
    wbRvfi.rvfiValid .= True
    wbRvfi.rvfiOrder <~ wbNRet <<+= 1
    case instr of
      WbRegWr rdAddr wr -> do
        wbRvfi.rvfiRdAddr .= rdAddr
        rdData <- wbRvfi.rvfiRdWData <.= guardZero rdAddr wr
        scribe toRd $ First $ Just (rdAddr, rdData)
      WbNop -> return ()
    scribe toRvfi . First . Just =<< use wbRvfi
  wbIR .= Nothing -- reset instruction, ready for next
  where
    guardZero 0 = const 0
    guardZero _ = id

memory :: RWS ToPipe FromPipe Pipe ()
memory = use meIR >>= \instrM ->
  forM_ instrM $ \case
    MeRegWr rd wr -> do
      wbRvfi <~ use meRvfi
      wbIR ?= WbRegWr rd wr
      meIR .= Nothing
    MeNop -> 
      wbRvfi <~ use meRvfi
      wbIR ?= WbNop
      meIR .= Nothing
    MeStore addr mask value -> do
      busStatus <- uses bus
      when (busStatus == Idling || busStatus == Memorizing) $ do
        busStatus .= Memorizing
        scribe toMem $ First $ Just $ DataMem addr mask $ Just value
        memM <- view fromMem
        forM_ memM $ const $ do
          wbRvfi <~ use meRvfi
          wbRvfi.rvfiMemAddr  .= addr
          wbRvfi.rvfiMemWMask .= mask
          wbRvfi.rvfiMemWData .= value
          wbIR ?= WbNop
          meIR .= Nothing 
          busStatus .= Idling
    MeLoad rdAddr addr mask -> do
      busStatus <- uses bus
      when (busStatus == Idling || busStatus == Memorizing) $ do
        busStatus .= Memorizing
        scribe toMem $ First $ Just $ DataMem addr mask Nothing
        memM <- view fromMem
        forM_ memM $ \memData -> do
          wbRvfi <~ use meRvfi
          wbRvfi.rvfiMemAddr .= addr
          wbRvfi.rvfiMemRMask .= mask
          wbIR ?= WbRegWr rdAddr wr
          meIR .= Nothing
          busStatus .= Idling

execute :: RWS ToPipe FromPipe Pipe ()
execute = use exIR >>= \instrM ->
  forM_ instrM $ \instr -> do
    exRs1Data <~ guardZero exRs1Zero fromRs1
    exRs2Data <~ guardZero exRs2Zero fromRs2
    case instr of
      Ex op rd imm -> case op of
        Lui -> when memRdy $ do
          meRvfi <~ use exRvfi
          pc <- meRvfi.rvfiPcRData <<~ use exPC
          meRvfi.rvfiPcWData .= pc + 4
          meRvfi.rvfiRs1Data .= exRs1Data
          meRvfi.rvfiRs2Data .= exRs2Data
          meIR ?= MeRegWr rd imm
          exIR .= Nothing
        Auipc -> when memRdy $ do
          meRvfi <~ use exRvfi
          pc <- meRvfi.rvfiPcRData <<~ use exPC
          meRvfi.rvfiPcWData .= pc + 4
          meIR ?= MeRegWr rd (pc + imm) 
          exIR .= Nothing
        Jal -> when memRdy $ do
          meRvfi <~ use exRvfi
          pc <- meRvfi.rvfiPcRData <<~ use exPC
          npc <- meRvfi.rvfiPcWData <.= pc + imm
          meRvfi.rvfiTrap ||= (npc .&. 0x3 /= 0)
          control.branching ?= npc
          meIR ?= MeRegWr rd (pc + 4)
          exIR .= Nothing
        Jalr -> when memRdy $ do
          meRvfi <~ use exRvfi
          pc <- meRvfi.rvfiPcRData <<~ use exPC
          npc <- meRvfi.rvfiPcWData <.= clearBit (rs1Data + imm) 0
          meRvfi.rvfiTrap ||= (npc .&. 0x3 /= 0)
          control.branching ?= npc
          meIR ?= MeRegWr rd (pc + 4)
          exIR .= Nothing
      ExBranch op imm -> when (memRdy && not fetching) $ do
        meRvfi <~ use exRvfi
        pc <- meRvfi.rvfiPcRData <<~ use exPC
        isBranch <- branch op <$> use exRs1Data <*> use exRs2Data
        npc <- meRvfi.rvfiPcWData <<~ if isBranch
                                        then do
                                          control.branching ?= (pc + imm)
                                          return $ pc + imm
                                        else return $ pc + 4
        meRvfi.rvfiTrap ||= (npc .&. 0x3 /= 0)
        meIR ?= MeNop
        exIR .= Nothing
      ExStore op imm -> when memRdy $ do
        meRvfi <~ use exRvfi
        pc <- meRvfi.rvfiPcRData <<~ use exPC
        meRvfi.rvfiPcWData .= pc + 4
        addr <- uses exRs1Data (+ imm)      -- unaligned
        let addr' = addr .&. complement 0x3 -- aligned
        case op of
          Sb -> do
            wr <- concatBitVector# . replicate d4 . slice d7 d0 <$> use exRs2Data
            meIR ?= MeStore addr' (byteMask addr) wr
          Sh -> do
            meRvfi.rvfiTrap ||= (addr .&. 0x1 /= 0)
            wr <- concatBitVector# . replicate d2 . slice d15 d0 <$> use exRs2Data
            meIR ?= MeStore addr' (halfMask addr) wr
          Sw -> do
            meRvfi.rvfiTrap ||= (addr .&. 0x3 /= 0)
            meIR ?= MeStore addr' 0xF rs2Data
        exIR .= Nothing
      ExLoad op rdAddr imm -> do
        _
        case op of
          Lb  -> _
          Lh  -> _
          Lw  -> _
          Lbu -> _
          Lhu -> _
      ExAlu    op rd     -> return $ Just $ MeRegWr rd $ alu op rs1Data rs2Data
      ExAluImm op rd imm -> return $ Just $ MeRegWr rd $ alu op rs1Data imm
  where
    guardZero rsZero rsValue = do
      isZero <- use rsZero
      if isZero
        then return 0
        else view rsValue

decode :: RWS ToPipe FromPipe Pipe ()
decode = use deIR >>= \memM ->
  forM_ memM $ \mem -> do
    use (control.branching) >>= \case
      Just _ -> deIR .= Nothing -- insert bubble
      Nothing -> when exRdy $ case parseInstr mem of
        Right instr -> do
          exRvfi .= mkRvfi
          exIR ?= instr
          exPC <~ use dePC
          exRvfi.rvfiInsn .= mem
          scribe toRs1Addr . First . Just =<< exRvfi.rvfiRs1Addr <.= sliceRs1 mem
          scribe toRs2Addr . First . Just =<< exRvfi.rvfiRs2Addr <.= sliceRs2 mem
          deIR .= Nothing
        Left _ -> deIR .= Nothing -- exRvfi.rvfiTrap .= True
  
fetch :: RWS ToPipe FromPipe Pipe ()
fetch = use control >>= \case
  Idle -> control .= Fetching
  Fetching -> do
    scribe toMem . First . Just =<< uses fetchPC InstrMem
    deIR .= Nothing
    memM <- use fromMem
    forM_ memM $ \mem -> do
      deIR ?= mem
      dePC <~ fetchPC <<+= 4
      control .= Idle
  Branching pc -> do
    control .= Fetching
    fetchPC .= pc
  Memorizing -> return ()

-------------
-- Utility --
-------------

-- | calcluate byte mask based on address
byteMask :: BitVector 32 -> BitVector 4
byteMask = (1 `shiftL`) . unpack . resize . slice d1 d0

-- | calculate half word mask based on address
halfMask :: BitVector 32 -> BitVector 4
halfMask addr = if addr .&. 0x2 == 0
                  then 0x3
                  else 0xC