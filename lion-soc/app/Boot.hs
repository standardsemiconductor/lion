{-# LANGUAGE LambdaCase #-}

import Control.Concurrent.Async (concurrently_)
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as C
import System.Environment (getArgs)
import System.Hardware.Serialport (
  CommSpeed(CS19200),
  SerialPortSettings(..),
  defaultSerialSettings,
  hWithSerial
  )
import System.IO (Handle, hGetChar, hPutChar)

main :: IO ()
main = boot =<< parseArgs <$> getArgs

parseArgs :: [String] -> (FilePath, Maybe FilePath)
parseArgs = \case
  [serialPath] -> (serialPath, Nothing)
  [serialPath, imagePath] -> (serialPath, Just imagePath)
  _ -> ("/dev/ttyUSB0", Just "_build/demo/led/led.bin")

boot :: (FilePath, Maybe FilePath) -> IO ()
boot (serialPath, imagePathM) =
  hWithSerial serialPath serialSettings $ \serialHandle ->
    case imagePathM of
      Nothing -> hPutChar serialHandle 'e'
      Just imagePath -> do
        hPutChar serialHandle 'n'
        C.hPut serialHandle =<< C.readFile imagePath

serialSettings :: SerialPortSettings
serialSettings = defaultSerialSettings{ commSpeed = CS19200 }

interactUart :: Handle -> IO ()
interactUart hndl = concurrently_ readUart writeUart
  where
    readUart  = forever $ putChar =<< hGetChar hndl
    writeUart = forever $ hPutChar hndl =<< getChar
