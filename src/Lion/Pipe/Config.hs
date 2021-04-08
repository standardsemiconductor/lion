{-|
Module      : Lion.Pipe.Config
Description : Pipeline configuration
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com
-}
module Lion.Pipe.Config where

import Clash.Prelude

-- | Pipeline configuration
data PipeConfig (startPC :: Nat) (aluConfig :: AluConfig) = PipeConfig
  deriving stock (Generic, Show, Eq)

-- | Default pipeline configuration
-- 
-- `startPC` = 0
defaultPipeConfig :: PipeConfig 0 'Soft
defaultPipeConfig = PipeConfig

-- | ALU configuration
data AluConfig = Hard -- ^ use hard adder and subtractor from iCE40 SB_MAC16
               | Soft -- ^ use generic adder and subtractor: (+) and (-)
  deriving stock (Generic, Show, Eq)
