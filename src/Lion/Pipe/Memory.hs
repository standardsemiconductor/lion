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
-- Pass NOP onto writeback stage.
meNop :: PipeM (a :: AluConfig) ()
meNop = wbIR ?= WbNop

---------------------------
-- Memory Register Write --
---------------------------
-- Push the ALU output and register destination address
-- to writeback stage.
-- Forward register with memory control.
class MemoryRegWr (a :: AluConfig) where
  meRegWr :: MeRegWrAlu a -> PipeM (a :: AluConfig)

instance MemoryRegWr 'Hard where
  meRegWr (MeRegWrAluHard rd) = do
    wr <- view fromAlu
    control.meRegFwd ?= (rd, wr)
    wbIR ?= WbRegWr rd wr

instance MemoryRegWr 'Soft where
  meRegWr (MeRegWrAluSoft rd wr) = do
    control.meRegFwd ?= (rd, wr)
    wbIR ?= WbRegWr rd wr

-----------------
-- Memory Jump --
-----------------
-- Set memory control branching and forward register.
-- Send register write to writeback stage.
meJump :: Unsigned 5 -> BitVector 32 -> PipeM (a :: AluConfig) ()
meJump rd pc4 = do
  control.meBranching .= True
  control.meRegFwd ?= (rd, pc4)
  wbIR ?= WbRegWr rd pc4

-------------------
-- Memory Branch --
-------------------
-- Set memory control branching.
-- Send NOP to writeback stage.
meBranch :: PipeM (a :: AluConfig) ()
meBranch = do
  control.meBranching .= True
  wbIR ?= WbNop

------------------
-- Memory Store --
------------------
meStore :: BitVector 32 -> BitVector 4 -> BitVector 32 -> PipeM (a :: AluConfig) ()
meStore addr mask value = do
  control.meMemory .= True
  scribe toMem $ First $ Just $ dataMem addr mask $ Just value
  wbRvfi.rvfiMemAddr  .= addr
  wbRvfi.rvfiMemWMask .= mask
  wbRvfi.rvfiMemWData .= value
  wbIR ?= WbStore

-----------------
-- Memory Load --
-----------------
meLoad 
  :: Load 
  -> Unsigned 5 
  -> BitVector 32 
  -> BitVector 4 
  -> PipeM (a :: AluConfig) ()
meLoad op rd addr mask = do
  control.meMemory .= True
  scribe toMem $ First $ Just $ dataMem addr mask Nothing
  wbRvfi.rvfiMemAddr  .= addr
  wbRvfi.rvfiMemRMask .= mask
  wbIR ?= WbLoad op rdAddr mask

---------------------------
-- Memory pipeline stage --
---------------------------
memory :: PipeM (a :: AluConfig) ()
memory = do
  wbIR   .= Nothing
  wbRvfi <~ use meRvfi
  withInstr meIR $ \case
    MeNop -> meNop
    MeRegWr regWr -> meRegWr regWr
    MeJump rd pc4 -> meJump rd pc4
    MeBranch -> meBranch
    MeStore addr mask value -> meStore addr mask value
    MeLoad op rdAddr addr mask -> meLoad op rdAddr addr mask
