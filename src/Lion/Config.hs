{-|
Module      : Lion.Config
Description : Lion configuration
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com
-}

module Lion.Config where

import Clash.Prelude
{-
-- | Branch configuration
data BranchConfig = BranchConfigAlu -- ^ branching occurs in memory stage, branch address is computed with ALU
                  | BranchConfigNoAlu -- ^ branching occurs in memory stage, branch address is computed without ALU
-}
-- | Pipeline configuration
data PipeConfig b = PipeConfig
  { startPC :: BitVector 32 -- ^ initial Program Counter address
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

-- | Default pipeline configuration
--
-- defaultPipeConfig :: PipeConfig BranchAlu
-- defaultPipeConfig = PipeConfig
--   { startPC = 0 }     
defaultPipeConfig :: PipeConfig MeBranchAlu
defaultPipeConfig = PipeConfig
  { startPC = 0
  }