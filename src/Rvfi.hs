module Rvfi where

import Clash.Prelude
import Control.Lens
import Data.Maybe
import Data.Monoid
import Data.Monoid.Generic
import Types

data RvfiCsr n = RvfiCsr
  { _wdataCsr :: First (BitVector n)
  , _rdataCsr :: First (BitVector n)
  , _wmaskCsr :: First (BitVector n)
  , _rmaskCsr :: First (BitVector n)
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
  deriving Semigroup via GenericSemigroup (RvfiCsr n)
  deriving Monoid via GenericMonoid (RvfiCsr n)
makeLenses ''RvfiCsr

data RvfiCsrOut n = RvfiCsrOut
  { _wdataCsrOut :: "wdata" ::: BitVector n
  , _rdataCsrOut :: "rdata" ::: BitVector n
  , _wmaskCsrOut :: "wmask" ::: BitVector n
  , _rmaskCsrOut :: "rmask" ::: BitVector n
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''RvfiCsrOut

fromFirst :: a -> First a -> a
fromFirst a = fromMaybe a . getFirst

fromRvfiCsr :: KnownNat n => RvfiCsr n -> RvfiCsrOut n
fromRvfiCsr rvfiCsr = RvfiCsrOut
  { _wdataCsrOut = fromFirst 0 $ rvfiCsr^.wdataCsr
  , _rdataCsrOut = fromFirst 0 $ rvfiCsr^.rdataCsr
  , _wmaskCsrOut = fromFirst 0 $ rvfiCsr^.wmaskCsr
  , _rmaskCsrOut = fromFirst 0 $ rvfiCsr^.rmaskCsr
  }

data Rvfi = Rvfi
  { _rvfiValid       :: First Bool
  , _rvfiOrder       :: First (Unsigned 64)
  , _rvfiInsn        :: First (W 32)
  , _rvfiTrap        :: First Bool
  , _rvfiHalt        :: First Bool
  , _rvfiIntr        :: First Bool
  , _rvfiMode        :: First (BitVector 2)
  , _rvfiIxl         :: First (BitVector 2)
  , _rvfiRs1Addr     :: First (Unsigned 5)
  , _rvfiRs2Addr     :: First (Unsigned 5)
  , _rvfiRs1Data     :: First (W 32)
  , _rvfiRs2Data     :: First (W 32)
  , _rvfiRdAddr      :: First (Unsigned 5)
  , _rvfiRdWData     :: First (W 32)
  , _rvfiPcRData     :: First PC
  , _rvfiPcWData     :: First PC
  , _rvfiMemAddr     :: First (W 32)
  , _rvfiMemRMask    :: First (BitVector 4)
  , _rvfiMemWMask    :: First (BitVector 4)
  , _rvfiMemRData    :: First (W 32)
  , _rvfiMemWData    :: First (W 32)
  , _rvfiCsrMinstret :: RvfiCsr 64
  , _rvfiCsrMcycle   :: RvfiCsr 64
  , _rvfiCsrMscratch :: RvfiCsr 32
  , _rvfiCsrMstatus  :: RvfiCsr 32
  , _rvfiCsrMisa     :: RvfiCsr 32
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
  deriving Semigroup via GenericSemigroup Rvfi
  deriving Monoid via GenericMonoid Rvfi
makeLenses ''Rvfi

data RvfiOut = RvfiOut
  { _rvfiValidOut       :: "valid"        ::: Bool
  , _rvfiOrderOut       :: "order"        ::: Unsigned 64
  , _rvfiInsnOut        :: "insn"         ::: W 32
  , _rvfiTrapOut        :: "trap"         ::: Bool
  , _rvfiHaltOut        :: "halt"         ::: Bool
  , _rvfiIntrOut        :: "intr"         ::: Bool
  , _rvfiModeOut        :: "mode"         ::: BitVector 2
  , _rvfiIxlOut         :: "ixl"          ::: BitVector 2
  , _rvfiRs1AddrOut     :: "rs1_addr"     ::: Unsigned 5
  , _rvfiRs2AddrOut     :: "rs2_addr"     ::: Unsigned 5
  , _rvfiRs1DataOut     :: "rs1_rdata"    ::: W 32
  , _rvfiRs2DataOut     :: "rs2_rdata"    ::: W 32
  , _rvfiRdAddrOut      :: "rd_addr"      ::: Unsigned 5
  , _rvfiRdWDataOut     :: "rd_wdata"     ::: W 32
  , _rvfiPcRDataOut     :: "pc_rdata"     ::: PC
  , _rvfiPcWDataOut     :: "pc_wdata"     ::: PC
  , _rvfiMemAddrOut     :: "mem_addr"     ::: W 32
  , _rvfiMemRMaskOut    :: "mem_rmask"    ::: BitVector 4
  , _rvfiMemWMaskOut    :: "mem_wmask"    ::: BitVector 4
  , _rvfiMemRDataOut    :: "mem_rdata"    ::: W 32
  , _rvfiMemWDataOut    :: "mem_wdata"    ::: W 32
  , _rvfiCsrMinstretOut :: "csr_minstret" ::: RvfiCsrOut 64
  , _rvfiCsrMcycleOut   :: "csr_mcycle"   ::: RvfiCsrOut 64
  , _rvfiCsrMscratchOut :: "csr_mscratch" ::: RvfiCsrOut 32
  , _rvfiCsrMstatusOut  :: "csr_mstatus"  ::: RvfiCsrOut 32
  , _rvfiCsrMisaOut     :: "csr_misa"     ::: RvfiCsrOut 32
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''RvfiOut

fromRvfi :: Rvfi -> RvfiOut
fromRvfi rvfi = RvfiOut
  { _rvfiValidOut       = fromFirst False $ rvfi^.rvfiValid
  , _rvfiOrderOut       = fromFirst 0     $ rvfi^.rvfiOrder
  , _rvfiInsnOut        = fromFirst 0     $ rvfi^.rvfiInsn
  , _rvfiTrapOut        = fromFirst False $ rvfi^.rvfiTrap
  , _rvfiHaltOut        = fromFirst False $ rvfi^.rvfiHalt
  , _rvfiIntrOut        = fromFirst False $ rvfi^.rvfiIntr
  , _rvfiModeOut        = fromFirst 3     $ rvfi^.rvfiMode
  , _rvfiIxlOut         = fromFirst 1     $ rvfi^.rvfiIxl
  , _rvfiRs1AddrOut     = fromFirst 0     $ rvfi^.rvfiRs1Addr
  , _rvfiRs2AddrOut     = fromFirst 0     $ rvfi^.rvfiRs2Addr
  , _rvfiRs1DataOut     = fromFirst 0     $ rvfi^.rvfiRs1Data
  , _rvfiRs2DataOut     = fromFirst 0     $ rvfi^.rvfiRs2Data
  , _rvfiRdAddrOut      = fromFirst 0     $ rvfi^.rvfiRdAddr
  , _rvfiRdWDataOut     = fromFirst 0     $ rvfi^.rvfiRdWData
  , _rvfiPcRDataOut     = fromFirst 0     $ rvfi^.rvfiPcRData
  , _rvfiPcWDataOut     = fromFirst 0     $ rvfi^.rvfiPcWData
  , _rvfiMemAddrOut     = fromFirst 0     $ rvfi^.rvfiMemAddr
  , _rvfiMemRMaskOut    = fromFirst 0     $ rvfi^.rvfiMemRMask
  , _rvfiMemWMaskOut    = fromFirst 0     $ rvfi^.rvfiMemWMask
  , _rvfiMemRDataOut    = fromFirst 0     $ rvfi^.rvfiMemRData
  , _rvfiMemWDataOut    = fromFirst 0     $ rvfi^.rvfiMemWData
  , _rvfiCsrMinstretOut = fromRvfiCsr     $ rvfi^.rvfiCsrMinstret
  , _rvfiCsrMcycleOut   = fromRvfiCsr     $ rvfi^.rvfiCsrMcycle
  , _rvfiCsrMscratchOut = fromRvfiCsr     $ rvfi^.rvfiCsrMscratch
  , _rvfiCsrMstatusOut  = fromRvfiCsr     $ rvfi^.rvfiCsrMstatus
  , _rvfiCsrMisaOut     = fromRvfiCsr     $ rvfi^.rvfiCsrMisa
  }