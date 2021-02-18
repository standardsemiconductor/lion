module Config where

import Clash.Prelude

data Width = W32 | W64
  deriving stock (Generic, Show, Eq)

data Mode = Normal | Verify
  deriving stock (Generic, Show, Eq)

data Config = Config
  { mode  :: Mode
  , ext   :: Extension
  , width :: Width
  }
  deriving stock (Generic, Show, Eq)

