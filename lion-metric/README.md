# Lion Metric

Observe Yosys synthesis metrics on the Lion Core.

## Prerequisites
* [Project IceStorm](https://github.com/standardsemiconductor/VELDT-info#project-icestorm): IceStorm Tools and Yosys

## Usage
generate metrics: `cabal run`

clean: `cabal run metric -- clean`

## Metrics as of Jan 17 2024 for iCE40
```
=== Metric ===

   Number of wires:               2831
   Number of wire bits:         380325
   Number of public wires:        2831
   Number of public wire bits:  380325
   Number of memories:               0
   Number of memory bits:            0
   Number of processes:              0
   Number of cells:               2968
     SB_CARRY                      368
     SB_DFF                         72
     SB_DFFER                       64
     SB_DFFR                       266
     SB_LUT4                      2194
     SB_RAM40_4K                     4
```
