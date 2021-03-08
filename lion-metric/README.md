# Lion Metric

Observe Yosys synthesis metrics on the Lion Core.

## Prerequisites
* [Project IceStorm](https://github.com/standardsemiconductor/VELDT-info#project-icestorm): IceStorm Tools and Yosys

## Usage
generate metrics: `cabal run`

clean: `cabal run metric -- clean`

## Metrics as of Mar 7 2021 for iCE40
```
=== Metric ===

   Number of wires:               2917
   Number of wire bits:         702072
   Number of public wires:        2917
   Number of public wire bits:  702072
   Number of memories:               0
   Number of memory bits:            0
   Number of processes:              0
   Number of cells:               2760
     SB_CARRY                      305
     SB_DFF                        262
     SB_DFFE                        74
     SB_DFFSR                       34
     SB_LUT4                      2081
     SB_RAM40_4K                     4
```