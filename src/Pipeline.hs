module Pipeline where

import Clash.Prelude

pipeline :: MonadRWS r w s m => m ()
pipeline = _