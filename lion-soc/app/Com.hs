
import System.Hardware.Serialport
import System.IO
import Control.Monad ( forever )

main :: IO ()
main = hWithSerial "/dev/ttyUSB0" serialPortSettings $ \hndl -> do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  forever $ echo hndl
  where
    echo hndl = do
      hPutChar hndl =<< getChar

serialPortSettings :: SerialPortSettings
serialPortSettings = defaultSerialSettings{ commSpeed = CS19200 }