module Lion.Util.Clash where

import Clash.Prelude

signResize, zeroResize :: forall m n . (KnownNat m, KnownNat n) => BitVector m -> BitVector n
signResize = resize
zeroResize = pack . (resize :: Unsigned m -> Unsigned n) . unpack

resizeTo :: (Resize a, KnownNat m) => SNat n -> a m -> a n
resizeTo SNat = resize

sign :: KnownNat n => BitVector n -> Signed n
sign = unpack

concatReplicateI :: forall m n . (KnownNat m, KnownNat n, 1 <= m) => BitVector m -> BitVector n
concatReplicateI = resize . concatBitVector# . replicate (SNat :: SNat (Div n m + 1))

slice' :: (BitPack a, KnownNat m) => SNat m -> SNat n -> a -> BitVector ((m + 1) - n)
slice' m n = slice m n . resizeTo (addSNat m d1) . pack
