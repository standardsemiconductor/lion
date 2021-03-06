{-|
Module      : Metric
Description : Lion Core Metric
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com

Core top entity to observe yosys metrics when synthesized for iCE40
-}

module Metric where

import Clash.Prelude
import Clash.Annotations.TH
import Lion.Core

metric :: HiddenClockResetEnable dom => Signal dom (BitVector 32) -> Signal dom (Maybe ToMem)
metric = toMem . core defaultPipeConfig
         
{-# NOINLINE topEntity #-}
topEntity 
  :: "clk"     ::: Clock System 
  -> "fromMem" ::: Signal System (BitVector 32)
  -> "toMem"   ::: Signal System (Maybe ToMem) 
topEntity clk = withClockResetEnable clk rst enableGen metric
  where
    rst = unsafeFromHighPolarity $ pure False
makeTopEntityWithName 'topEntity "Metric"
