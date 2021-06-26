# Lion Metric

Observe Yosys synthesis metrics on the Lion Core.

## Prerequisites
* [Project IceStorm](https://github.com/standardsemiconductor/VELDT-info#project-icestorm): IceStorm Tools and Yosys

## Usage
generate metrics: `cabal run`

clean: `cabal run metric -- clean`

## Metrics as of Jun 24 2021 for iCE40
```
=== Metric ===

   Number of wires:               2305
   Number of wire bits:         632631
   Number of public wires:        2305
   Number of public wire bits:  632631
   Number of memories:               0
   Number of memory bits:            0
   Number of processes:              0
   Number of cells:               2702
     SB_CARRY                      370
     SB_DFFER                       64
     SB_DFFR                       266
     SB_LUT4                      1998
     SB_RAM40_4K                     4
```
