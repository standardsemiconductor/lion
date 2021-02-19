module Types where

import Clash.Prelude

newtype W n = W { unW :: BitVector n }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX, BitPack)
  deriving newtype Num

newtype PC = PC { unPC :: W 32 }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX, BitPack)
  deriving newtype Num