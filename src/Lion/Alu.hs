{-| 
Module : Lion.Alu
Description : Lion arithmetic logic unit
Copyright : (c) David Cox, 2021
License : BSD-3-Clause
Maintainer : standardsemiconductor@gmail.com

Configurable alu, choose between soft and hard adders/subtractors
-}

module Lion.Alu where

import Clash.Prelude
import Data.Function ( on )
import Ice40.Mac 
import Lion.Instruction

-- | Alu configuration
data AluConfig = Hard -- ^ use hard adder and subtractor from iCE40 SB_MAC16
               | Soft -- ^ use generic adder and subtractor: (+), (-)
  deriving stock (Generic, Show, Eq)

-- | Configurable arithmetic logic unit
alu 
  :: HiddenClockResetEnable dom
  => AluConfig 
  -> Signal dom Op
  -> Signal dom (BitVector 32)
  -> Signal dom (BitVector 32)
  -> Signal dom (BitVector 32)
alu = \case
  Soft -> softAlu
  Hard -> hardAlu

-- | Soft adder/subtractor
softAlu
  :: HiddenClockResetEnable dom
  => Signal dom Op
  -> Signal dom (BitVector 32)
  -> Signal dom (BitVector 32)
  -> Signal dom (BitVector 32)
softAlu = liftA3 $ \case
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

-- | Use SB_MAC16 for addition and subtraction
hardAlu
  :: HiddenClock dom
  => Signal dom Op
  -> Signal dom (BitVector 32)
  -> Signal dom (BitVector 32)
  -> Signal dom (BitVector 32)
hardAlu op in1 in2 = mux isAddSub addSubOut $ liftA3 softie op in1 in2
  where
    isAdd = (Add == ) <$> op
    isSub = (Sub == ) <$> op
    isAddSub = isAdd .||. isSub
    addSubOut = hardAddSub (boolToBit <$> isSub) in1 in2
    softie = \case 
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

-- | addSub32BypassedUnsigned
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
    parameter = defaultParameter{ mode8x8 = 1
                                , botAddSubUpperInput = 1
                                , topAddSubCarrySelect = 2
                                , topAddSubUpperInput = 1
                                }
{- 
      macPrim 0 -- negTrigger
              0 -- aReg
              0 -- bReg
              0 -- cReg
              0 -- dReg
              0 -- top8x8MultReg
              0 -- bot8x8MultReg
              0 -- pipeline16x16MultReg1
              0 -- pipeline16x16MultReg2
              0 -- topOutputSelect
              0 -- topAddSubLowerInput
              1 -- topAddSubUpperInput
              2 -- topAddSubCarrySelect
              0 -- botOutputSelect
              0 -- botAddSubLowerInput
              1 -- botAddSubUpperInput
              0 -- botAddSubCarrySelect
              1 -- mode8x8
              0 -- aSigned
              0 -- bSigned
              hasClock -- clk  
              1 -- ce
              (slice d31 d16 <$> x) -- c
              (slice d31 d16 <$> y) -- a
              (slice d15 d0  <$> y) -- b
              (slice d15 d0  <$> x) -- d
              0 -- irsttop
              0 -- irstbot
              0 -- orsttop
              0 -- orstbot
              0 -- ahold
              0 -- bhold
              0 -- chold
              0 -- dhold
              0 -- oholdtop
              0 -- oholdbot
              addSub -- addsubtop
              addSub -- addsubbot
              0 -- oloadtop
              0 -- oloadbot
              0 -- accumci
              0 -- signextin
              0 -- ci
-}