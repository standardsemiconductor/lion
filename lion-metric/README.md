# Lion Metric

Observe Yosys synthesis metrics on the Lion Core.

## Prerequisites
* [Project IceStorm](https://github.com/standardsemiconductor/VELDT-info#project-icestorm): IceStorm Tools and Yosys

## Usage
generate metrics: `cabal run`

clean: `cabal run metric -- clean`

## Metrics as of 03/03/2021 for iCE40
```
=== Metric ===

   Number of wires:               2320
   Number of wire bits:         577169
   Number of public wires:        2320
   Number of public wire bits:  577169
   Number of memories:               0
   Number of memory bits:            0
   Number of processes:              0
   Number of cells:               3111
     SB_CARRY                      491
     SB_DFF                        259
     SB_DFFE                        42
     SB_DFFSR                        2
     SB_LUT4                      2313
     SB_RAM40_4K                     4
```