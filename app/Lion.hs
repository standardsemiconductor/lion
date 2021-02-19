module Main where

import Development.Shake
import Development.Shake.FilePath
import Clash.Main (defaultMain)

top :: String
top = "LionFV"

buildDir :: FilePath
buildDir = "_build"

buildDir' :: FilePath -> FilePath
buildDir' = (buildDir </>)

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=buildDir} $ do
  want [buildDir' ("verilog" </> top </> top </> top <.> "v")]

  phony "clean" $ do
    putInfo "Cleaning files in _build"
    removeFilesAfter "_build" ["//*"]

  buildDir' ("verilog" </> top </> top </> top <.> "v") %> \_ ->
    liftIO $ defaultMain ["-fclash-hdldir", buildDir, top, "--verilog"]
