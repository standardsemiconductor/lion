
import Control.Concurrent ( forkIO )
import Control.Monad      ( forever )
import Data.Functor       ( (<&>) )
import System.Environment ( getArgs )
import System.Hardware.Serialport 
import System.IO 

main :: IO ()
main = com =<< portPath
  where
    portPath = getArgs <&> \case
      [pathArg] -> pathArg
      _         -> "/dev/ttyUSB0" 

com :: String -> IO ()
com portPath = hWithSerial portPath serialPortSettings $ \hndl -> do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  forkIO $ readUart hndl
  writeUart hndl
  where
    readUart  hndl = forever $ putChar =<< hGetChar hndl
    writeUart hndl = forever $ hPutChar hndl =<< getChar

serialPortSettings :: SerialPortSettings
serialPortSettings = defaultSerialSettings{ commSpeed = CS19200 }