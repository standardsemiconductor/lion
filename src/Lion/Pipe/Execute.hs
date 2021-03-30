module Lion.Pipe.Execute where

import Clash.Prelude

class Execute (a :: AluConfig) where
  execute :: RWS (ToPipe a) (FromPipe a) (Pipe a) ()

class ExecuteEx (a :: AluConfig) where
  ex 
    :: Op 
    -> Unsigned 5   -- rd
    -> BitVector 32 -- pc
    -> BitVector 32 -- imm
    -> RWS (ToPipe a) (FromPipe a) (Pipe a) ()

instance Ex 'Hard where
  ex op rd pc imm = do
    case op of
      Lui   -> scribeAlu Add 0 imm
      Auipc -> scribeAlu Add pc imm  
    meIR ?= MeRegWr rd

