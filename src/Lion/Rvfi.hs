{-|
Module      : Lion.Rvfi
Description : Lion RISC-V Formal Verification Interface
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com

As the pipeline processes instructions, it populates the fields of the Rvfi data. When the instruction reaches the end of the pipeline, the core retires the instruction and writes the Rvfi data to output. This output is inspected and verified by the riscv-formal framework. See [riscv-formal](https://github.com/standardsemiconductor/riscv-formal) for more information about the interface. To verify the Lion core, see [lion-formal](https://github.com/standardsemiconductor/lion/tree/main/lion-formal).
-}

module Lion.Rvfi where

import Clash.Prelude
import Control.Lens
import Data.Maybe
import Data.Monoid

-- | RISC-V Formal Csr Interface
data RvfiCsr n = RvfiCsr
  { _wdataCsr :: "wdata" ::: BitVector n
  , _rdataCsr :: "rdata" ::: BitVector n
  , _wmaskCsr :: "wmask" ::: BitVector n
  , _rmaskCsr :: "rmask" ::: BitVector n
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''RvfiCsr

-- | RISC-V Formal Interface
data Rvfi = Rvfi
  { -- | When the core retires an instruction, it asserts the `rvfiValid` signal
    -- and uses the signals described in Rvfi to output the details of the
    -- retired instruction. The signals below are only valid during such a
    -- cycle and can be driven to arbitrary values in a cycle in which `_rvfiValid
    -- is not asserted.
    _rvfiValid       :: "valid"        ::: Bool

    -- | The `rvfiOrder` field must be set to the instruction index. No indices
    -- must be used twice and there must be no gaps. Instructions may be retired
    -- in a reordered fashion, as long as causality is preserved
    -- (register nad memory write operations must be retired before the read
    -- operations that depend on them).
  , _rvfiOrder       :: "order"        ::: BitVector 64

    -- | `rvfiInsn` is the instruction word for the retired instruction.
    -- In case of an instruction with fewer than ILEN bits, the upper bits
    -- of this output must all be zero. For compressed instructions the
    -- compressed instruction word must be output on this port.
    -- For fused instructions the complete fused instruciton sequence must
    -- be output
  , _rvfiInsn        :: "insn"         ::: BitVector 32

    -- | `rvfiTrap` must be set for an instruction that cannot be decoded
    -- as a legal instruction, such as 0x00000000.
  , _rvfiTrap        :: "trap"         ::: Bool

    -- | The signal `rvfiHalt` must be set when the instruction is
    -- the last instruction that the core retires before halting execution.
    -- It should not be set for an instruction that triggers a trap condition
    -- if the CPU reacts to the trap by executing a trap handler. This signal
    -- enable verification of liveness properties.
  , _rvfiHalt        :: "halt"         ::: Bool

    -- | `rvfiIntr` must be set for the first instruction that is part of a
    -- trap handler, i.e. an instruction that has a `rvfiPcRData` that
    -- does not match the `rvfiPcWData` of the previous instruction.
  , _rvfiIntr        :: "intr"         ::: Bool

    -- | `rvfiMode` must be set to the current privilege level, using the
    -- following encoding: 0=U-Mode, 1=S-Mode, 2=Reserved, 3=M-Mode
  , _rvfiMode        :: "mode"         ::: BitVector 2

    -- | `rvfiIxl` must be set to the value of MXL/SXL/UXL in the current
    -- privilege level, using the following encoding: 1=32, 2=64
  , _rvfiIxl         :: "ixl"          ::: BitVector 2

    -- | `rvfiRs1Addr` and `rvfiRs2Addr` are the decoded rs1 and rs2
    -- register addresses for the retired instruction. For an instruction
    -- that reads no rs1/rs2 register, this output can have an arbitrary value.
    -- However if this output is nonzero then `rvfiRs1Data` or `rvfiRs2Data` must
    -- carry the value stored in that register in the pre-state.
  , _rvfiRs1Addr     :: "rs1_addr"     ::: Unsigned 5
  , _rvfiRs2Addr     :: "rs2_addr"     ::: Unsigned 5

    -- | `rvfiRs1Data` and `rvfiRs2Data` are the values of the register
    -- addressed by rs1 and rs2 before execute of this instruction.
    -- This output must be zero when rs1/rs2 is zero.
  , _rvfiRs1Data     :: "rs1_rdata"    ::: BitVector 32
  , _rvfiRs2Data     :: "rs2_rdata"    ::: BitVector 32

    -- | `rvfiRdAddr` is the decoded rd register address for the retired
    -- instruction. For an instruction that writes no rd register, this output
    -- must always be zero.
  , _rvfiRdAddr      :: "rd_addr"      ::: Unsigned 5

    -- | `rvfiRdWData` is the value of the register addressed by rd after
    -- execution of this instruction. This output must be zero when rd
    -- is zero.
  , _rvfiRdWData     :: "rd_wdata"     ::: BitVector 32

    -- | This is the program counter (pc) before (`rvfiPcRData`) and after
    -- (`rvfiPcWData`) execution of this instruciton. I.e. this is the
    -- address of the retired instruction and the address of the next
    -- instruction.
  , _rvfiPcRData     :: "pc_rdata"     ::: BitVector 32
  , _rvfiPcWData     :: "pc_wdata"     ::: BitVector 32

    -- | For memory operations (`rvfiMemRMask` and/or `rvfiMemWMask` are non-zero),
    -- `rvfiMemAddr` holds the accessed memory location.
  , _rvfiMemAddr     :: "mem_addr"     ::: BitVector 32

    -- | `rvfiMemRMask` is a bitmask that specifies which bytes in `rvfiMemRData`
    -- contain valid read data from `rvfiMemAddr`.
  , _rvfiMemRMask    :: "mem_rmask"    ::: BitVector 4

    -- | `rvfiMemWMask` is a bitmask that specifies which bytes in `rvfiMemWData` 
    -- contain valid data this is written to `rvfiMemAddr`.
  , _rvfiMemWMask    :: "mem_wmask"    ::: BitVector 4

    -- | `rvfiMemRData` is the pre-state data read from `rvfiMemAddr`.
    -- `rvfiMemRMask` specifies which bytes are valid.
  , _rvfiMemRData    :: "mem_rdata"    ::: BitVector 32

    -- | `rvfiMemWData` is the post-state data written to `rvfiMemAddr`.
    -- `rvfiMemWMask` specifies which bytes are valid.
  , _rvfiMemWData    :: "mem_wdata"    ::: BitVector 32

  , _rvfiCsrMinstret :: "csr_minstret" ::: RvfiCsr 64
  , _rvfiCsrMcycle   :: "csr_mcycle"   ::: RvfiCsr 64
  , _rvfiCsrMscratch :: "csr_mscratch" ::: RvfiCsr 32
  , _rvfiCsrMstatus  :: "csr_mstatus"  ::: RvfiCsr 32
  , _rvfiCsrMisa     :: "csr_misa"     ::: RvfiCsr 32
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''Rvfi

-- | Unwrap Rvfi from First monoid
fromRvfi :: First Rvfi -> Rvfi
fromRvfi = fromMaybe mkRvfi . getFirst

-- | Construct the RISC-V Formal Interface
mkRvfi :: Rvfi 
mkRvfi = Rvfi
  { _rvfiValid       = False
  , _rvfiOrder       = 0    
  , _rvfiInsn        = 0    
  , _rvfiTrap        = False
  , _rvfiHalt        = False
  , _rvfiIntr        = False
  , _rvfiMode        = 3    
  , _rvfiIxl         = 1    
  , _rvfiRs1Addr     = 0    
  , _rvfiRs2Addr     = 0    
  , _rvfiRs1Data     = 0    
  , _rvfiRs2Data     = 0    
  , _rvfiRdAddr      = 0    
  , _rvfiRdWData     = 0    
  , _rvfiPcRData     = 0    
  , _rvfiPcWData     = 0    
  , _rvfiMemAddr     = 0    
  , _rvfiMemRMask    = 0    
  , _rvfiMemWMask    = 0    
  , _rvfiMemRData    = 0    
  , _rvfiMemWData    = 0    
  , _rvfiCsrMinstret = RvfiCsr 0 0 0 0
  , _rvfiCsrMcycle   = RvfiCsr 0 0 0 0
  , _rvfiCsrMscratch = RvfiCsr 0 0 0 0 
  , _rvfiCsrMstatus  = RvfiCsr 0 0 0 0
  , _rvfiCsrMisa     = RvfiCsr 0 0 0 0
  }
