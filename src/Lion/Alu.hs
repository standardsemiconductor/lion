{-| 
Module      : Lion.Alu
Description : Lion arithmetic logic unit
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com

Configurable alu, choose between soft and hard adders/subtractors
-}

module Lion.Alu where

import Clash.Prelude
import Data.Function ( on )
import Data.Proxy
import Ice40.Mac 
import Lion.Pipe.Instruction

-- | ALU configuration
data AluConfig = Hard -- ^ use hard adder and subtractor from iCE40 SB_MAC16
               | Soft -- ^ use generic adder and subtractor: (+) and (-)
  deriving stock (Generic, Show, Eq)

class Alu (config :: AluConfig) where
  alu :: HiddenClockResetEnable dom 
      => Proxy (config :: AluConfig)
      -> Signal dom Op
      -> Signal dom (BitVector 32)
      -> Signal dom (BitVector 32)
      -> Signal dom (BitVector 32)

instance Alu 'Soft where
  alu _ op in1 = register 0 . liftA3 aluFunc op in1 
    where
      aluFunc = \case 
        Add  -> (+)
        Sub  -> (-)
        Sll  -> \x y -> x `shiftL` shamt y
        Slt  -> boolToBV ... (<) `on` sign
        Sltu -> boolToBV ... (<)
        Xor  -> xor
        Srl  -> \x y -> x `shiftR` shamt y
        Sra  -> \x y -> pack $ sign x `shiftR` shamt y
        Or   -> (.|.)
        And  -> (.&.)
        where
          shamt = unpack . resize . slice d4 d0
          sign = unpack :: BitVector 32 -> Signed 32
          (...) = (.).(.)
      
instance Alu 'Hard where
  alu _ op in1 in2 = mux isAddSub adderSubtractor $ register 0 $ baseAlu op in1 in2
    where
      isAdd = (Add == ) <$> op
      isSub = (Sub == ) <$> op
      isAddSub = delay False $ isAdd .||. isSub
      adderSubtractor = hardAddSub (boolToBit <$> isSub) in1 in2
  
baseAlu
  :: Signal dom Op
  -> Signal dom (BitVector 32)
  -> Signal dom (BitVector 32)
  -> Signal dom (BitVector 32)
baseAlu = liftA3 $ \case 
  Add  -> \_ _ -> 0
  Sub  -> \_ _ -> 0
  Sll  -> \x y -> x `shiftL` shamt y
  Slt  -> boolToBV ... (<) `on` sign
  Sltu -> boolToBV ... (<)
  Xor  -> xor
  Srl  -> \x y -> x `shiftR` shamt y
  Sra  -> \x y -> pack $ sign x `shiftR` shamt y
  Or   -> (.|.)
  And  -> (.&.)
  where
    shamt = unpack . resize . slice d4 d0
    sign = unpack :: BitVector 32 -> Signed 32
    (...) = (.).(.)

-- | addSub32PipelinedUnsigned
hardAddSub
  :: HiddenClock dom
  => Signal dom Bit -- 0 = Add, 1 = Sub
  -> Signal dom (BitVector 32)
  -> Signal dom (BitVector 32)
  -> Signal dom (BitVector 32)
hardAddSub addSub x y = out
  where
    (out, _, _, _) = mac parameter input
    input = defaultInput{ a = slice d31 d16 <$> y
                        , b = slice d15 d0  <$> y
                        , c = slice d31 d16 <$> x
                        , d = slice d15 d0  <$> x
                        , addsubtop = addSub
                        , addsubbot = addSub
                        }
    parameter = defaultParameter{ topOutputSelect = 1
                                , topAddSubUpperInput = 1
                                , topAddSubCarrySelect = 2
                                , botOutputSelect = 1
                                , botAddSubUpperInput = 1
                                , mode8x8 = 1
                                }