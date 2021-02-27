module Main where

import Data.Word

import Development.Shake
import Development.Shake.FilePath
import Clash.Main (defaultMain)
import Control.Monad (when, forM_)
import System.Directory (withCurrentDirectory)
import Text.Printf (printf)
import qualified Data.ByteString as BS

formalTop :: String
formalTop = "LionFV"

socTop :: String
socTop = "Soc"

buildDir :: FilePath
buildDir = "_build"

buildDir' :: FilePath -> FilePath
buildDir' = (buildDir </>)

main :: IO ()
main = shakeArgs opts $ do

  want ["_build/verilog" </> socTop </> socTop </> socTop <.> "v"]

  phony "clean" $ do
    putInfo "Cleaning files in _build"
    removeFilesAfter "_build" ["//*"]
    putInfo "Cleaning riscv-formal checks"
    removeFilesAfter "riscv-formal/cores/lion/checks" ["//*"]

  phony "formal" $ do
    putInfo "Formal Verification of Lion Core"
    need [buildDir' ("verilog" </> formalTop </> formalTop </> formalTop <.> "v")]

    -- generate checks
    liftIO $ withCurrentDirectory "riscv-formal/cores/lion" $
      cmd_ "python3" [".." </> ".." </> "checks" </> "genchecks.py"]

    -- gather checks
    checks <- getDirectoryFiles "riscv-formal/cores/lion/checks" ["//*.sby"]

    -- verify checks
    _ <- forP checks $ \check -> do
      let checkNoExt = dropExtensions check
      cmd_ "make" "-C" ["riscv-formal/cores/lion/checks"] [checkNoExt]
      need ["riscv-formal/cores/lion/checks" </> checkNoExt </> "PASS"]
      putInfo $ show checkNoExt ++ ": PASS"
    putInfo "All formal checks PASS"

  -- build formal 
  buildDir' ("verilog" </> formalTop </> formalTop </> formalTop <.> "v") %> \_ ->
    liftIO $ compile formalTop

  -- build soc
  buildDir' ("verilog" </> socTop </> socTop </> socTop <.> "v") %> \_ -> do
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
  when (len <= 256) $ do
    let bs' = zip (cycle [0,1,2,3]) $ BS.unpack $ bs `BS.append` BS.replicate (4*256 - len) 0
        rom = unlines $ map (asBits.snd) $ filter ((== i).fst) bs'
    liftIO $ writeFile ("_build/bios/bios" <.> ("rom" ++ show i)) rom
  where
    asBits :: Word8 -> String
    asBits = printf "%08b" 
    
