# Lion SoC

System-On-Chip using Lion and targeting the VELDT FPGA development board.

## Prerequisites
* [Project IceStorm](https://github.com/standardsemiconductor/VELDT-info#project-icestorm)
* [riscv-gnu-toolchain](https://github.com/riscv/riscv-gnu-toolchain)

## Usage
1. Ensure the VELDT is ON and in the FLASH mode.
2. `cabal run soc -- prog` 

To compile, synthesize, and route without programming:
`cabal run`

## Clean
`cabal run soc -- clea