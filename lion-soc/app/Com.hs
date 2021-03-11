
import System.Hardware.Serialport
import System.IO
import Control.Concurrent ( forkIO )
import Control.Monad      ( forever )

main :: IO ()
main = hWithSerial "/dev/ttyUSB0" serialPortSettings $ \hndl -> do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  forkIO $ readUart hndl
  writeUart hndl
  where
    readUart hndl = forever $ do
      putChar =<< hGetChar hndl
    writeUart hndl = forever $ do
      hPutChar hndl =<< getChar

serialPortSettings :: SerialPortSettings
serialPortSettings = defaultSerialSettings{ commSpeed = CS19200 }