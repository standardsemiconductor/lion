module Main where

import Development.Shake
import Development.Shake.FilePath
import Clash.Main (defaultMain)

metricTop :: String
metricTop = "Metric"

buildDir :: FilePath
buildDir = "_build"

buildDir' :: FilePath -> FilePath
buildDir' = (buildDir </>)

verilog :: FilePath
verilog = "_build/verilog/Metric/Metric"

main :: IO ()
main = shakeArgs opts $  do
  want ["_build/Metric.json"]

  phony "clean" $ do
    putInfo "Cleaning files in _build"
    removeFilesAfter "_build" ["//*"]

  -- yosys synthesis
  "_build/Metric.json" %> \out -> do
    putInfo "Syntehsizing Metric"
    need [verilog </> metricTop <.> "v"]
    cmd_ "yosys"
         "-p"
         ["synth_ice40 -top " ++ metricTop ++ " -json " ++ out ++ " -retime -abc2"]
         [verilog </> "*.v"]

  -- compile Metric
  verilog </> metricTop <.> "v" %> \_ ->
    liftIO $ compile metricTop

  where
    opts = shakeOptions{ shakeFiles = buildDir }
      
compile :: String -> IO ()
compile topModule = defaultMain ["-fclash-hdldir", buildDir, topModule, "--verilog"]