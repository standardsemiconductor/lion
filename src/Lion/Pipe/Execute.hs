module Lion.Pipe.Execute where

import Clash.Prelude

class ExecuteEx (a :: AluConfig) where
  executeEx :: Op -- op
            -> Unsigned 5 -- rd
            -> BitVector 32 -- imm
            -> BitVector 32 -- pc
            -> RWS (ToPipe a) (FromPipe a) (Pipe a) ()

class ExecuteExJump (a :: AluConfig) where
  executeExJump :: Jump
                -> Unsigned 5 -- rd
                -> BitVector 32 -- imm
                -> BitVector 32 -- pc
                -> BitVector 32 -- pc4
                -> BitVector 32 -- rs1Data
                -> RWS (ToPipe a) (FromPipe a) (Pipe a) ()

class ExecuteExBranch (a :: AluConfig) where
  executeExBranch :: RWS (ToPipe a) (FromPipe a) (Pipe a) ()

class ExecuteExStore (a :: AluConfig) where
  executeExStore 

class Execute (a :: AluConfig) where
  execute :: RWS (ToPipe a) (FromPipe a) (Pipe a) ()

instance Execute (a :: AluConfig) where
  execute = do
    meIR .= Nothing
    meRvfi <~ use exRvfi
    pc  <- meRvfi.rvfiPcRData <<~ use exPC
    pc4 <- meRvfi.rvfiPcWData <.= pc + 4
    rs1Data <- meRvfi.rvfiRs1Data <<~ regFwd exRs1 fromRs1 (control.meRegFwd) (control.wbRegFwd)
    rs2Data <- meRvfi.rvfiRs2Data <<~ regFwd exRs2 fromRs2 (control.meRegFwd) (control.wbRegFwd)
    withInstr exIR $ \case
      Ex op rd imm         -> executeEx op rd imm pc
      ExJump jump rd imm   -> executeExJump jump rd imm pc pc4 rs1Data
      ExBranch op imm      -> executeExBranch op imm rs1Data rs2Data pc pc4
      ExStore op imm       -> executeExStore op imm rs1Data rs2Data
      ExLoad op rdAddr imm -> executeExLoad op rdAddr imm rs1Data
      ExAlu op rd          -> executeExAlu op rd rs1Data rs2Data
      ExAluImm op rd imm   -> executeExAluImm op rd imm rs1Data
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

class ExecuteEx (a :: AluConfig) where
  ex 
    :: Op 
    -> Unsigned 5   -- rd
    -> BitVector 32 -- pc
    -> BitVector 32 -- imm
    -> RWS (ToPipe a) (FromPipe a) (Pipe a) ()

instance Ex 'Hard where
  ex op rd pc imm = do
    case op of
      Lui   -> scribeAlu Add 0 imm
      Auipc -> scribeAlu Add pc imm  
    meIR ?= MeRegWr rd

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

