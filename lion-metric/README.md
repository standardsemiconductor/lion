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

   Number of wires:               2879
   Number of wire bits:         657683
   Number of public wires:        2879
   Number of public wire bits:  657683
   Number of memories:               0
   Number of memory bits:            0
   Number of processes:              0
   Number of cells:               2913
     SB_CARRY                      369
     SB_DFF                        289
     SB_DFFE                        44
     SB_DFFSR                       34
     SB_LUT4                      2173
     SB_RAM40_4K                     4
```