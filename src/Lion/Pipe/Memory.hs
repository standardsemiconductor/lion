{-|
Module      : Lion.Pipe.Memory
Description : Pipeline Memory Stage
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com
-}

module Lion.Pipe.Memory where

import Clash.Prelude

----------------
-- Memory NOP --
----------------
meNop :: PipeM (a :: AluConfig) ()
meNop = wbIR ?= WbNop

-----------------
-- Memory Jump --
-----------------
meJump :: Unsigned 5 -> BitVector 32 -> PipeM (a :: AluConfig) ()
meJump rd pc4 = do
  control.meBranching .= True
  control.meRegFwd ?= (rd, pc4)
  wbIR ?= WbRegWr rd pc4

-------------------
-- Memory Branch --
-------------------
meBranch :: PipeM (a :: AluConfig) ()
meBranch = do
  control.meBranching .= True
  wbIR ?= WbNop

------------------
-- Memory Store --
------------------
meStore :: BitVector 32 -> BitVector 4 -> BitVector 32 -> PipeM (a :: AluConfig) ()
meStore addr mask value = do
class Memory (a :: AluConfig) where
  memory :: RWS (ToPipe a) (FromPipe a) (Pipe a) ()
    
instance Memory 'Hard where
  memory = do
    wbIR   .= Nothing
    wbRvfi <~ use meRvfi
    withInstr meIR $ \case
      MeNop -> meNop
      MeRegWr rd -> do
        wr <- view fromAlu
        control.meRegFwd ?= (rd, wr)
        wbIR ?= WbRegWr rd wr
      MeJump rd pc4 -> meJump rd pc4
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

-- | Memory stage
memory :: RWS ToPipe FromPipe Pipe ()
memory = do
  wbIR   .= Nothing
  wbRvfi <~ use meRvfi
  withInstr meIR $ \case
    MeNop -> wbIR ?= WbNop
    MeRegWr rd -> do
      wr <- view fromAlu
      control.meRegFwd ?= (rd, wr)
      wbIR ?= WbRegWr rd wr
    MeJump rd pc4 -> do
      control.meBranching .= True
      control.meRegFwd ?= (rd, pc4)
      wbIR ?= WbRegWr rd pc4
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
