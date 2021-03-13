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
buildDir = "_build"

buildDir' :: FilePath -> FilePath
buildDir' = (buildDir </>)

verilog :: String -> FilePath
verilog top = "_build" </> top <.> "topEntity"

main :: IO ()
main = shakeArgs opts $ do

  want ["_build/Soc.bin"]

  phony "clean" $ do
    putInfo "Cleaning files in _build"
    removeFilesAfter "_build" ["//*"]

  phony "soc" $ do
    putInfo "Synthesizing Soc"
    need ["_build/Soc.bin"]

  phony "prog" $ do
    putInfo "Programming"
    need ["_build/Soc.bin"]
    cmd_ "iceprog" "_build/Soc.bin"

  phony "bios" $ do
    need [ "_build/bios/bios.rom0"
         , "_build/bios/bios.rom1"
         , "_build/bios/bios.rom2"
         , "_build/bios/bios.rom3"
         ]

  -- yosys synthesis
  "_build/Soc.json" %> \out -> do
    putInfo "Synthesizing Soc"
    need [verilog socTop </> socTop <.> "v"]
    cmd_ "yosys" 
         "-q" 
         "-p" 
         ["synth_ice40 -top " ++ socTop ++ " -json " ++ out ++ " -retime -abc2"] 
         [verilog socTop </> "*.v"]

  -- place and route NextPNR
  "_build/Soc.asc" %> \out -> do
    putInfo "Place and Route Soc"
    need ["_build/Soc.json", "Soc.pcf"]
    cmd_ "nextpnr-ice40"
         "--up5k"
         "--opt-timing"
         "--package sg48"
         "--pcf Soc.pcf"
         "--asc"
         [out]
         "--json _build/Soc.json"
  
  -- ice pack
  "_build/Soc.bin" %> \out -> do
    putInfo "Ice Pack"
    need ["_build/Soc.asc"]
    cmd_ "icepack" "_build/Soc.asc" [out]
    
  -- build soc
  verilog socTop </> socTop <.> "v" %> \_ -> do
    need [ "_build/bios/bios.rom0"
         , "_build/bios/bios.rom1"
         , "_build/bios/bios.rom2"
         , "_build/bios/bios.rom3"
         ]
    liftIO $ compile socTop  

  "_build/bios/bios.rom*" %> \out -> do
    need ["_build/bios/bios.bin"]
    buildRom $ read $ drop 4 $ takeExtension out 

  "_build/bios/bios.bin" %> \out -> do
    need ["_build/bios/bios.o"]
    cmd_ "riscv64-unknown-elf-objcopy" "-O" "binary" "_build/bios/bios.o" [out]

  "_build/bios/bios.o" %> \out -> do
    need ["bios/bios.S", "bios/bios.linker"]
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
         "-Wl,-T,bios/bios.linker"
         "bios/bios.S"
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
  bs <- liftIO $ BS.readFile "_build/bios/bios.bin"
  let len = BS.length bs
  if len <= 1024
    then do
      let bs' = zip (cycle [0,1,2,3]) $ BS.unpack $ bs `BS.append` BS.replicate (4*256 - len) 0
          rom = unlines $ map (asBits.snd) $ filter ((== i).fst) bs'
      liftIO $ writeFile ("_build/bios/bios" <.> ("rom" ++ show i)) rom
    else error $ "bios too large for rom: length = " ++ show len
  where
    asBits :: Word8 -> String
    asBits = printf "%08b" 
    
