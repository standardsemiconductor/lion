module Main where

import Data.Word

import Development.Shake
import Development.Shake.FilePath
import Clash.Main (defaultMain)
import Control.Monad (when)
import Text.Printf (printf)
import qualified Data.ByteString as BS

socTop :: String
socTop = "Soc"

buildDir :: FilePath
buildDir = "soc/_build"

buildDir' :: FilePath -> FilePath
buildDir' = (buildDir </>)

verilog :: String -> FilePath
verilog top = "soc/_build/verilog" </> top </> top

main :: IO ()
main = shakeArgs opts $ do

  phony "clean" $ do
    putInfo "Cleaning files in _build"
    removeFilesAfter "soc/_build" ["//*"]

  phony "soc" $ do
    putInfo "Synthesizing Soc"
    need ["soc/_build/Soc.bin"]

  phony "prog" $ do
    putInfo "Programming"
    need ["soc/_build/Soc.bin"]
    cmd_ "iceprog" "soc/_build/Soc.bin"

  -- yosys synthesis
  "soc/_build/Soc.json" %> \out -> do
    putInfo "Synthesizing Soc"
    need [verilog socTop </> socTop <.> "v"]
    cmd_ "yosys" 
         "-q" 
         "-p" 
         ["synth_ice40 -top " ++ socTop ++ " -json " ++ out ++ " -retime -abc2"] 
         [verilog socTop </> "*.v"]

  -- place and route NextPNR
  "soc/_build/Soc.asc" %> \out -> do
    putInfo "Place and Route Soc"
    need ["soc/_build/Soc.json", "soc/Soc.pcf"]
    cmd_ "nextpnr-ice40"
         "--up5k"
         "--package sg48"
         "--pcf soc/Soc.pcf"
         "--asc"
         [out]
         "--json soc/_build/Soc.json"
  
  -- ice pack
  "soc/_build/Soc.bin" %> \out -> do
    putInfo "Ice Pack"
    need ["soc/_build/Soc.asc"]
    cmd_ "icepack" "soc/_build/Soc.asc" [out]
    
  -- build soc
  verilog socTop </> socTop <.> "v" %> \_ -> do
    need [ "soc/_build/bios/bios.rom0"
         , "soc/_build/bios/bios.rom1"
         , "soc/_build/bios/bios.rom2"
         , "soc/_build/bios/bios.rom3"
         ]
    liftIO $ compile socTop  

  "soc/_build/bios/bios.rom*" %> \out -> do
    need ["soc/_build/bios/bios.bin"]
    buildRom $ read $ drop 4 $ takeExtension out 

  "soc/_build/bios/bios.bin" %> \out -> do
    need ["soc/_build/bios/bios.o"]
    cmd_ "riscv64-unknown-elf-objcopy" "-O" "binary" "soc/_build/bios/bios.o" [out]

  "soc/_build/bios/bios.o" %> \out -> do
    need ["soc/bios/bios.S", "soc/bios/bios.linker"]
    cmd_ "riscv64-unknown-elf-gcc"
         "-march=rv32i"
         "-mabi=ilp32"
         "-g"
         "-ffreestanding"
         "-Os"
         "-Wl,--gc-sections"
         "-nostdlib"
         "-nostartfiles"
         "-nodefaultlibs"
         "-Wl,-T,soc/bios/bios.linker"
         "soc/bios/bios.S"
         "-o"
         [out]
  where
    opts = shakeOptions
      { shakeFiles = buildDir
      , shakeThreads = 0
      }

compile :: String -> IO ()
compile topModule = defaultMain  ["-fclash-hdldir", buildDir, topModule, "--verilog"]

buildRom :: Int -> Action ()
buildRom i = do
  bs <- liftIO $ BS.readFile "soc/_build/bios/bios.bin"
  let len = BS.length bs
  when (len <= 256) $ do
    let bs' = zip (cycle [0,1,2,3]) $ BS.unpack $ bs `BS.append` BS.replicate (4*256 - len) 0
        rom = unlines $ map (asBits.snd) $ filter ((== i).fst) bs'
    liftIO $ writeFile ("soc/_build/bios/bios" <.> ("rom" ++ show i)) rom
  where
    asBits :: Word8 -> String
    asBits = printf "%08b" 
    
