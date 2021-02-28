# Lion SoC

System-On-Chip using Lion and targeting the [VELDT FPGA development board](https://standardsemiconductor.com).

## Prerequisites
* [Project IceStorm](https://github.com/standardsemiconductor/VELDT-info#project-icestorm)
* [riscv-gnu-toolchain](https://github.com/riscv/riscv-gnu-toolchain)
  * Need `riscv64-unknown-*` binaries

## Usage
1. Ensure the VELDT is ON and in the FLASH mode.
2. `cabal run soc -- prog` 

To compile, synthesize, and route without programming: `cabal run`

## Clean
`cabal run soc -- clean`

## Memory Map
| Peripheral | Start Address | End Address |
|------------|---------------|-------------|
| Bios       |  0x00000000   | 0x000000FF  |
| Led        |  0x00000100   | 0x00000100  |

## Peripherals
### Led
|********|********|*  *  *  *  *  *  *  *|*  *  *  *  *  *  *  *|

|--resvd-|-resvd--|15----Reg Address-----|7-----Reg Data-------0|

See [Appendix D of the iCE40 LED Driver Usage Guide](https://github.com/standardsemiconductor/VELDT-info/blob/master/ICE40LEDDriverUsageGuide.pdf) for information on LED control bus addressable registers and register field descriptions.