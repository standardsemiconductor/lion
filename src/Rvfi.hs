module Rvfi where

import Clash.Prelude
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

fromRvfiCsr :: RvfiCsr n -> RvfiCsrOut n
fromRvfiCsr rvfiCsr = RvfiCsrOut
  { _wdataCsrOut = fromFirst 0 $ rvfiCsr^.wdataCsr
  , _rdataCsrOut = fromFirst 0 $ rvfiCsr^.rdataCsr
  , _wmaskCsrOut = fromFirst 0 $ rvfiCsr^.wmaskCsr
  , _rmaskCsrOut = fromFirst 0 $ rvfiCsr^.rmaskCsr
  }

data Rvfi = Rvfi
  { _rvfiValid            :: First Bool
  , _rvfiOrder            :: First (Unsigned 64)
  , _rvfiInsn             :: First (W 32)
  , _rvfiTrap             :: First Bool
  , _rvfiHalt             :: First Bool
  , _rvfiIntr             :: First Bool
  , _rvfiMode             :: First (BitVector 2)
  , _rvfiIxl              :: First (BitVector 2)
  , _rvfiRs1Addr          :: First RegAddr
  , _rvfiRs2Addr          :: First RegAddr
  , _rvfiRs1Data          :: First (W 32)
  , _rvfiRs2Data          :: First (W 32)
  , _rvfiRdAddr           :: First RegAddr
  , _rvfiRdWData          :: First (W 32)
  , _rvfiPcRData          :: First PC
  , _rvfiPcWData          :: First PC
  , _rvfiMemAddr          :: First MemAddr
  , _rvfiMemRMask         :: First (BitVector 4)
  , _rvfiMemWMask         :: First (BitVector 4)
  , _rvfiMemRData         :: First (W 32)
  , _rvfiMemWData         :: First (W 32)
  , _rvfiCsrMinstret      :: RvfiCsr 64
  , _rvfiCsrMcycle        :: RvfiCsr 64
  , _rvfiCsrMscratch      :: RvfiCsr 32
  , _rvfiCsrMstatus       :: RvfiCsr 32
  , _rvfiCsrMisa          :: RvfiCsr 32
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
  deriving Semigroup via GenericSemigroup (Rvfi n)
  deriving Monoid via GenericMonoid (Rvfi n)
makeLenses ''Rvfi

rvfiInit :: Rvfi
rvfiInit = Rvfi
  { _rvfiValid    = False  :: Bool
  , _rvfiOrder    = 0      :: Unsigned 64
  , _rvfiInsn     = 0      :: Word
  , _rvfiTrap     = False  :: Bool
  , _rvfiHalt     = False  :: Bool
  , _rvfiIntr     = False  :: Bool
  , _rvfiMode     = 3      :: BitVector 2
  , _rvfiIxl      = 1      :: BitVector 2
  , _rvfiRs1Addr  = 0      :: RegAddr
  , _rvfiRs2Addr  = 0      :: RegAddr
  , _rvfiRs1Data  = 0      :: Word
  , _rvfiRs2Data  = 0      :: Word
  , _rvfiRdAddr   = 0      :: RegAddr
  , _rvfiRdWData  = 0      :: Word
  , _rvfiPcRData  = 0      :: PC
  , _rvfiPcWData  = 0      :: PC
  , _rvfiMemAddr  = 0      :: MemAddr
  , _rvfiMemRMask = 0b0000 :: BitVector 4
  , _rvfiMemWMask = 0b0000 :: BitVector 4
  , _rvfiMemRData = 0      :: Word
  , _rvfiMemWData = 0      :: Word
  , _rvfiCsrMinstret = mkRvfiCsr :: RvfiCsr 64
  , _rvfiCsrMcycle   = mkRvfiCsr :: RvfiCsr 64
  , _rvfiCsrMscratch = mkRvfiCsr :: RvfiCsr 32
  , _rvfiCsrMstatus  = mkRvfiCsr :: RvfiCsr 32
  , _rvfiCsrMisa     = mkRvfiCsr :: RvfiCsr 32
  }

data RvfiOut = RvfiOut
  { _rvfiValidOut            :: "valid"              ::: Bool
  , _rvfiOrderOut            :: "order"              ::: Unsigned 64
  , _rvfiInsnOut             :: "insn"               ::: W 32
  , _rvfiTrapOut             :: "trap"               ::: Bool
  , _rvfiHaltOut             :: "halt"               ::: Bool
  , _rvfiIntrOut             :: "intr"               ::: Bool
  , _rvfiModeOut             :: "mode"               ::: BitVector 2
  , _rvfiIxlOut              :: "ixl"                ::: BitVector 2
  , _rvfiRs1AddrOut          :: "rs1_addr"           ::: RegAddr
  , _rvfiRs2AddrOut          :: "rs2_addr"           ::: RegAddr
  , _rvfiRs1DataOut          :: "rs1_rdata"          ::: W 32
  , _rvfiRs2DataOut          :: "rs2_rdata"          ::: W 32
  , _rvfiRdAddrOut           :: "rd_addr"            ::: RegAddr
  , _rvfiRdWDataOut          :: "rd_wdata"           ::: W 32
  , _rvfiPcRDataOut          :: "pc_rdata"           ::: PC
  , _rvfiPcWDataOut          :: "pc_wdata"           ::: PC
  , _rvfiMemAddrOut          :: "mem_addr"           ::: MemAddr
  , _rvfiMemRMaskOut         :: "mem_rmask"          ::: BitVector 4
  , _rvfiMemWMaskOut         :: "mem_wmask"          ::: BitVector 4
  , _rvfiMemRDataOut         :: "mem_rdata"          ::: W 32
  , _rvfiMemWDataOut         :: "mem_wdata"          ::: W 32
  , _rvfiCsrMinstretOut      :: "csr_minstret"       ::: RvfiCsr 64
  , _rvfiCsrMcycleOut        :: "csr_mcycle"         ::: RvfiCsr 64
  , _rvfiCsrMscratchOut      :: "csr_mscratch"       ::: RvfiCsr 32
  , _rvfiCsrMstatusOut       :: "csr_mstatus"        ::: RvfiCsr 32
  , _rvfiCsrMisaOut          :: "csr_misa"           ::: RvfiCsr 32
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''RvfiOut

fromRvfi :: Rvfi -> RvfiOut
fromRvfi rvfi = RvfiOut
  { _rvfiValidOut = fromFirst _ $ rvfi^.rvfiValid
  , _rvfiOrderOut = fromFirst _ $ rvfi^.rvfiOrder
  , _rvfiInsnOut  = fromFirst _ $ rvfi^.rvfiInsn
  , _rvfiTrapOut  = fromFirst _ $ rvfi^.rvfiTrap
  }