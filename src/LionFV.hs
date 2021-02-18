module LionFV where

import Clash.Prelude
import Clash.Annotations.TH

import Pipeline ( pipeline )

lionFV
  :: HiddenClockResetEnable dom
  => Signal dom FromMem
  -> Unbundled dom (ToMem, Rvfi)
lionFV = runConfig pipeline defaultConfig{ mode = Verify }