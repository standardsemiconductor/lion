# Lion Metric

Observe Yosys synthesis metrics on the Lion Core.

## Prerequisites
* [Project IceStorm](https://github.com/standardsemiconductor/VELDT-info#project-icestorm): IceStorm Tools and Yosys

## Usage
generate metrics: `cabal run`

clean: `cabal run metric -- clean`

## Metrics as of Mar 30 2021 for iCE40
```
=== Metric ===

   Number of wires:               2345
   Number of wire bits:         646900
   Number of public wires:        2345
   Number of public wire bits:  646900
   Number of memories:               0
   Number of memory bits:            0
   Number of processes:              0
   Number of cells:               2703
     SB_CARRY                      370
     SB_DFFER                       64
     SB_DFFR                       266
     SB_LUT4                      1999
     SB_RAM40_4K                     4
```