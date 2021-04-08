module Lion.Pipe.Execute where

import Clash.Prelude
import Control.Lens
import Lion.Pipe.Instruction
import Lion.Pipe.Monad

----------------
-- Execute Ex --
----------------
class ExecuteEx (a :: AluConfig) where
  executeEx :: Op -- op
            -> Unsigned 5 -- rd
            -> BitVector 32 -- imm
            -> BitVector 32 -- pc
            -> RWS (ToPipe a) (FromPipe a) (Pipe a) ()

instance Ex 'Hard where
  executeEx op rd pc imm = do
    case op of
      Lui   -> scribeAlu Add 0 imm
      Auipc -> scribeAlu Add pc imm  
    meIR ?= MeRegWr rd

instance Ex 'Soft where
  executeEx op rd pc imm = meIR ?= case op of
    Lui   -> MeRegWr rd imm
    Auipc -> MeRegWr rd $ pc + imm

------------------
-- Execute Jump --
------------------
executeJump
  :: Jump
  -> Unsigned 5 -- rd
  -> BitVector 32 -- imm
  -> BitVector 32 -- pc
  -> BitVector 32 -- pc4
  -> BitVector 32 -- rs1Data
  -> RWS (ToPipe a) (FromPipe a) (Pipe a) ()
executeJump jump rd imm pc pc4 rs1Data = do
  npc <- meRvfi.rvfiPcWData <<~ control.exBranching <?= case jump of
    Jal  -> pc + imm
    Jalr -> clearBit (rs1Data + imm) 0
  meRvfi.rvfiTrap ||= isMisaligned npc
  meIR ?= MeJump rd pc4

--------------------
-- Execute Branch --
--------------------
executeBranch :: RWS (ToPipe a) (FromPipe a) (Pipe a) ()
executeBranch
  :: Branch
  -> BitVector 32 -- imm
  -> BitVector 32 -- rs1Data
  -> BitVector 32 -- rs2Data
  -> BitVector 32 -- pc
  -> BitVector 32 -- pc4
  -> RWS (ToPipe a) (FromPipe a) (Pipe a) ()
executeBranch op imm rs1Data rs2Data pc pc4 
  | branch op rs1Data rs2Data = do
      branchPC <- meRvfi.rvfiPcWData <<~ control.exBranching <?= pc + imm
      meRvfi.rvfiTrap ||= isMisaligned branchPC
      meIR ?= MeBranch
  | otherwise = do
      meRvfi.rvfiTrap ||= isMisaligned pc4
      meIR ?= MeNop

-------------------
-- Execute Store --
-------------------
executeStore
  :: Store
  -> BitVector 32 -- imm
  -> BitVector 32 -- rs1Data
  -> BitVector 32 -- rs2Data
  -> RWS (ToPipe a) (FromPipe a) (Pipe a) ()
executeStore op imm rs1Data rs2Data = case op of
  Sb -> let wr = concatBitVector# $ replicate d4 $ slice d7 d0 rs2Data
        in meIR ?= MeStore addr' (byteMask addr) wr
  Sh -> do
    meRvfi.rvfiTrap ||= isMisalignedHalf addr -- trap on half-word boundary
    let wr = concatBitVector# $ replicate d2 $ slice d15 d0 rs2Data
    meIR ?= MeStore addr' (halfMask addr) wr
  Sw -> do
    meRvfi.rvfiTrap ||= isMisaligned addr -- trap on word boundary
    meIR ?= MeStore addr' 0xF rs2Data
  where
    addr  = rs1Data + imm           -- unaligned
    addr' = addr .&. complement 0x3 -- aligned

------------------
-- Execute Load --
------------------
executeLoad
  :: Load 
  -> Unsigned 5   -- rd
  -> BitVector 32 -- imm
  -> BitVector 32 -- rs1Data
  -> RWS (ToPipe a) (FromPipe a) (Pipe a) ()
executeLoad op rd imm rs1Data = do
  control.exLoad .= True
  if | op == Lb || op == Lbu -> meIR ?= MeLoad op rdAddr addr' (byteMask addr)
     | op == Lh || op == Lhu -> do
         meRvfi.rvfiTrap ||= isMisalignedHalf addr -- trap on half-word boundary
         meIR ?= MeLoad op rdAddr addr' (halfMask addr)
     | otherwise -> do -- Lw
         meRvfi.rvfiTrap ||= isMisaligned addr -- trap on word boundary
         meIR ?= MeLoad op rdAddr addr' 0xF
  where
    addr  = rs1Data + imm            -- unaligned
    addr' = addr .&. complement 0x3 -- aligned

-----------------
-- Execute Alu --
-----------------
class ExecuteAlu (a :: AluConfig) where
  executeAlu
    :: Op
    -> Unsigned 5   -- rd
    -> BitVector 32 -- rs1Data
    -> BitVector 32 -- rs2Data
    -> RWS (ToPipe a) (FromPipe a) (Pipe a) ()

instance ExecuteAlu 'Hard where  
  executeAlu op rd rs1Data rs2Data = do
    scribeAlu op rs1Data rs2Data
    meIR ?= MeRegWr rd

instance ExecuteAlu 'Soft where
  executeAlu op rd rs1Data rs2Data = meIRM ?= MeRegWr rd (I.alu op rs1Data rs2Data)

---------------------
-- Execute Alu Imm --
---------------------
class ExecuteAluImm (a :: AluConfig) where
  executeAluImm
    :: Op
    -> Unsigned 5   -- rd
    -> BitVector 32 -- imm
    -> BitVector 32 -- rs1Data
    -> RWS (ToPipe a) (FromPipe a) (Pipe a) ()

instance ExecuteAluImm 'Hard where
  executeAluImm op rd imm rs1Data = do
    scribeAlu op rs1Data imm
    meIR ?= MeRegWr rd

instance ExecuteAluImm 'Soft where
  executeAluImm op rd imm rs1Data = meIR ?= MeRegWr rd (I.alu op rs1Data imm)

-- | Execute stage
execute 
  :: ExecuteEx a 
  => ExecuteAlu a
  => ExecuteAluImm a
  => RWS (ToPipe a) (FromPipe a) (Pipe a) ()
execute = do
  meIR .= Nothing
  meRvfi <~ use exRvfi
  pc  <- meRvfi.rvfiPcRData <<~ use exPC
  pc4 <- meRvfi.rvfiPcWData <.= pc + 4
  rs1Data <- meRvfi.rvfiRs1Data <<~ regFwd exRs1 fromRs1 (control.meRegFwd) (control.wbRegFwd)
  rs2Data <- meRvfi.rvfiRs2Data <<~ regFwd exRs2 fromRs2 (control.meRegFwd) (control.wbRegFwd)
  withInstr exIR $ \case
    Ex op rd imm         -> executeEx op rd imm pc
    ExJump jump rd imm   -> executeJump jump rd imm pc pc4 rs1Data
    ExBranch op imm      -> executeBranch op imm rs1Data rs2Data pc pc4
    ExStore op imm       -> executeStore op imm rs1Data rs2Data
    ExLoad op rdAddr imm -> executeLoad op rdAddr imm rs1Data
    ExAlu op rd          -> executeAlu op rd rs1Data rs2Data
    ExAluImm op rd imm   -> executeAluImm op rd imm rs1Data
  where
    regFwd 
      :: MonadState s m 
      => MonadReader r m
      => Lens' s (Unsigned 5) 
      -> Lens' r (BitVector 32)
      -> Lens' s (Maybe (Unsigned 5, BitVector 32))
      -> Lens' s (Maybe (Unsigned 5, BitVector 32))
      -> m (BitVector 32)
    regFwd rsAddr rsData meFwd wbFwd = 
      guardZero rsAddr =<< fwd <$> use rsAddr <*> view rsData <*> use meFwd <*> use wbFwd
      where
        guardZero  -- register x0 always has value 0.
          :: MonadState s m 
          => Lens' s (Unsigned 5) 
          -> BitVector 32 
          -> m (BitVector 32)
        guardZero rsAddr rsValue = do
          isZero <- uses rsAddr (== 0)
          return $ if isZero
            then 0
            else rsValue

scribeAlu op in1 in2 = do
  scribe toAluOp     $ First $ Just op
  scribe toAluInput1 $ First $ Just in1
  scribe toAluInput2 $ First $ Just in2
{-
-- | Execute stage
execute :: RWS ToPipe FromPipe (Pipe a) ()
execute = do
  meIR .= Nothing
  meRvfi <~ use exRvfi
  pc  <- meRvfi.rvfiPcRData <<~ use exPC
  pc4 <- meRvfi.rvfiPcWData <.= pc + 4
  rs1Data <- meRvfi.rvfiRs1Data <<~ regFwd exRs1 fromRs1 (control.meRegFwd) (control.wbRegFwd)
  rs2Data <- meRvfi.rvfiRs2Data <<~ regFwd exRs2 fromRs2 (control.meRegFwd) (control.wbRegFwd)
  withInstr exIR $ \case
    Ex op rd imm -> do
      case op of
        Lui   -> scribeAlu Add 0  imm
        Auipc -> scribeAlu Add pc imm
      meIR ?= MeRegWr rd 0
    ExJump jump rd imm -> do
      case jump of
        Jal -> do
          npc <- meRvfi.rvfiPcWData <<~ control.exBranching <?= pc + imm
          meRvfi.rvfiTrap ||= isMisaligned npc
          meIR ?= MeJump rd pc4
        Jalr -> do
          npc <- meRvfi.rvfiPcWData <<~ control.exBranching <?= clearBit (rs1Data + imm) 0
          meRvfi.rvfiTrap ||= isMisaligned npc
          meIR ?= MeJump rd pc4
    ExBranch op imm ->
      if branch op rs1Data rs2Data
        then do
          branchPC <- meRvfi.rvfiPcWData <<~ control.exBranching <?= pc + imm
          meRvfi.rvfiTrap ||= isMisaligned branchPC
          meIR ?= MeBranch
        else do
          meRvfi.rvfiTrap ||= isMisaligned pc4
          meIR ?= MeNop
    ExStore op imm -> do
      let addr = rs1Data + imm            -- unaligned
          addr' = addr .&. complement 0x3 -- aligned
      case op of
        Sb -> let wr = concatBitVector# $ replicate d4 $ slice d7 d0 rs2Data
              in meIR ?= MeStore addr' (byteMask addr) wr
        Sh -> do
          meRvfi.rvfiTrap ||= isMisalignedHalf addr -- trap on half-word boundary
          let wr = concatBitVector# $ replicate d2 $ slice d15 d0 rs2Data
          meIR ?= MeStore addr' (halfMask addr) wr
        Sw -> do
          meRvfi.rvfiTrap ||= isMisaligned addr -- trap on word boundary
          meIR ?= MeStore addr' 0xF rs2Data
    ExLoad op rdAddr imm -> do
      control.exLoad .= True
      let addr = rs1Data + imm            -- unaligned
          addr' = addr .&. complement 0x3 -- aligned
      if | op == Lb || op == Lbu -> meIR ?= MeLoad op rdAddr addr' (byteMask addr)
         | op == Lh || op == Lhu -> do
             meRvfi.rvfiTrap ||= isMisalignedHalf addr -- trap on half-word boundary
             meIR ?= MeLoad op rdAddr addr' (halfMask addr)
         | otherwise -> do -- Lw
             meRvfi.rvfiTrap ||= isMisaligned addr -- trap on word boundary
             meIR ?= MeLoad op rdAddr addr' 0xF
    ExAlu op rd -> do
--      scribeAlu op rs1Data rs2Data
--      meIR ?= MeRegWr rd
      exAlu rd op rs1Data rs2Data
    ExAluImm op rd imm -> do
      scribeAlu op rs1Data imm
      meIR ?= MeRegWr rd
  where
    scribeAlu op in1 in2 = do
      scribe toAluOp     $ First $ Just op
      scribe toAluInput1 $ First $ Just in1
      scribe toAluInput2 $ First $ Just in2

    guardZero  -- register x0 always has value 0.
      :: MonadState s m 
      => Lens' s (Unsigned 5) 
      -> BitVector 32 
      -> m (BitVector 32)
    guardZero rsAddr rsValue = do
      isZero <- uses rsAddr (== 0)
      return $ if isZero
        then 0
        else rsValue
    regFwd 
      :: MonadState s m 
      => MonadReader r m
      => Lens' s (Unsigned 5) 
      -> Lens' r (BitVector 32)
      -> Lens' s (Maybe (Unsigned 5, BitVector 32))
      -> Lens' s (Maybe (Unsigned 5, BitVector 32))
      -> m (BitVector 32)
    regFwd rsAddr rsData meFwd wbFwd = 
      guardZero rsAddr =<< fwd <$> use rsAddr <*> view rsData <*> use meFwd <*> use wbFwd

class PipeAlu (a :: AluConfig) where
  exAlu :: Proxy a
        -> Unsigned 5 -- rd
        -> Op
        -> BitVector 32 -- rs1Data
        -> BitVector 32 -- rs2Data
        -> RWS ToPipe FromPipe Pipe ()

  meAlu :: Proxy a
        -> 

instance PipeAlu 'Hard where
  exAlu _ rd op rs1Data rs2Data = do
    scribeAlu op rs1Data rs2Data
    meIR ?= MeRegWr rd 0
  
instance PipeAlu 'Soft where
  exAlu _ rd op rs1Data rs2Data = do
    let aluOut = alu op rs1Data rs2Data
    meIR ?= MeRegWr rd aluOut
-}
