{-|
Module      : Metric
Description : Lion Core Metric
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com

Core top entity to observe yosys metrics when synthesized for iCE40
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Metric where

import Clash.Prelude
import Clash.Annotations.TH
import Lion.Core

metric 
  :: HiddenClockResetEnable dom 
  => (KnownNat xl, KnownNat (Log2 xl), 1 <= Div xl 8)
  => Signal dom (BitVector xl) 
  -> Signal dom (Maybe (ToMem xl))
metric = toMem . core defaultCoreConfig
         
{-# NOINLINE topEntity #-}
topEntity 
  :: "clk"     ::: Clock System 
  -> "rst"     ::: Reset System
  -> "fromMem" ::: Signal System (BitVector 32)
  -> "toMem"   ::: Signal System (Maybe (ToMem 32)) 
topEntity clk rst = withClockResetEnable clk rst enableGen metric
makeTopEntityWithName 'topEntity "Metric"
