# Lion Metric

Observe Yosys synthesis metrics on the Lion Core.

## Prerequisites
* [Project IceStorm](https://github.com/standardsemiconductor/VELDT-info#project-icestorm): IceStorm Tools and Yosys

## Usage
generate metrics: `cabal run`

clean: `cabal run metric -- clean`

## Metrics as of Mar 16 2021 for iCE40
```
=== Metric ===

   Number of wires:               2534
   Number of wire bits:         708968
   Number of public wires:        2534
   Number of public wire bits:  708968
   Number of memories:               0
   Number of memory bits:            0
   Number of processes:              0
   Number of cells:               2720
     SB_CARRY                      305
     SB_DFF                         33
     SB_DFFER                       74
     SB_DFFR                       256
     SB_LUT4                      2048
     SB_RAM40_4K                     4
```