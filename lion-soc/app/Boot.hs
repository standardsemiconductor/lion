{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (
  Concurrently(Concurrently, runConcurrently),
  concurrently_,
  mapConcurrently_,
  race_
  )
import Control.Concurrent.STM (
  TChan,
  atomically,
  dupTChan,
  newBroadcastTChanIO,
  newTChan,
  newTChanIO,
  readTChan,
  writeTChan
  )
import Control.Monad (forever, forM_, join, void, (<=<))
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC (
  ByteString,
  getContents,
  hGetContents,
  hPut,
  pack,
  putStr,
  putStrLn,
  readFile,
  singleton,
  takeWhile
  )
import System.Environment (getArgs)
import System.Hardware.Serialport (
  CommSpeed(CS19200),
  SerialPortSettings(..),
  defaultSerialSettings,
  hWithSerial
  )
import System.IO (
  BufferMode(LineBuffering, NoBuffering),
  Handle,
  hGetChar,
  hPutChar,
  hSetBuffering,
  hSetNewlineMode,
  stdin,
  universalNewlineMode
  )

main :: IO ()
main = boot =<< parseArgs <$> getArgs

parseArgs :: [String] -> (FilePath, Maybe FilePath)
parseArgs = \case
  [serialPath] -> (serialPath, Nothing)
  [serialPath, imagePath] -> (serialPath, Just imagePath)
  _ -> ("/dev/ttyUSB0", Just "_build/demo/led/led.bin")

boot :: (FilePath, Maybe FilePath) -> IO ()
boot (serialPath, imagePathM) =
  hWithSerial serialPath serialSettings $ \serialHandle -> do
    hSetBuffering stdin NoBuffering
    hSetBuffering serialHandle NoBuffering
    hSetNewlineMode serialHandle universalNewlineMode
    interactSerial imagePathM =<< mkBoot serialHandle

data Boot = Boot
  { bootHandle      :: Handle
  , fromSerialTChan :: TChan LC.ByteString
  , toSerialTChan   :: TChan LC.ByteString
  }

mkBoot :: Handle -> IO Boot
mkBoot hndl = do
  fromSerial <- newBroadcastTChanIO
  toSerial   <- newTChanIO
  return $ Boot{ bootHandle      = hndl
               , fromSerialTChan = fromSerial
               , toSerialTChan   = toSerial
               }

interactSerial :: Maybe FilePath -> Boot -> IO ()
interactSerial imagePathM boot =
  mapConcurrently_
    ($ boot)
    [ fromSerial
    , toSerial
    , toStdout
    , setup imagePathM
    ]

fromSerial :: Boot -> IO ()
fromSerial boot = do
  bs <- LC.hGetContents $ bootHandle boot
  atomically $ writeTChan (fromSerialTChan boot) bs

toSerial :: Boot -> IO ()
toSerial boot = join $ atomically $ do
  bs <- readTChan $ toSerialTChan boot
  return $ LC.hPut (bootHandle boot) bs

fromStdin :: Boot -> IO ()
fromStdin boot = writeSerial boot =<< LC.getContents

toStdout :: Boot -> IO ()
toStdout boot = LC.putStr =<< readSerial boot
toStdout boot = join $ atomically $ do
  bs <- readTChan (fromSerialTChan boot)
  return $ LC.putStr bs

setup :: Maybe FilePath -> Boot -> IO ()
setup imagePathM boot = do
  write "a"
  --atomically $ do
  --  chan <- dupTChan $ fromSerialTChan boot
  --  _ <- LC.takeWhile (/= '?') <$> readTChan chan
  --  return ()
  --fmap (LC.dropWhile (/= '?')) $ readTChan =<< dupTChan (fromSerialTChan boot) 
  case imagePathM of
    Nothing -> write "e"
    Just path -> do
      write "n"
      LC.putStrLn $ "\nUploading " <> LC.pack path
      write =<< LC.readFile path
      LC.putStrLn "Done"
  fromStdin boot
  where
    write = writeSerial boot

writeSerial :: Boot -> LC.ByteString -> IO ()
writeSerial boot = atomically . writeTChan (toSerialTChan boot)

--    hPutChar serialHandle '\n'
--    threadDelay 1000
--    hPutChar serialHandle 'n'
--    forM_ imagePathM $ C.hPut serialHandle <=< C.readFile
--    case imagePathM of
--      Nothing -> hPutChar serialHandle 'e'
--      Just imagePath -> do
--        hPutChar serialHandle 'n'
--        C.hPut serialHandle =<< C.readFile imagePath
--    interactUart serialHandle

serialSettings :: SerialPortSettings
serialSettings = defaultSerialSettings{ commSpeed = CS19200 }
