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

### Clean
`cabal run soc -- clean`

## Metrics for iCE40 FPGA: Mar 7 2021
### Device utilisation
```
Device utilisation:
 ICESTORM_LC:  2438/ 5280    46%
ICESTORM_RAM:     8/   30    26%
       SB_IO:     1/   96     1%
       SB_GB:     4/    8    50%
 SB_LEDDA_IP:     1/    1   100%
 SB_RGBA_DRV:     1/    1   100%
```
### Clock frequency
```
Max frequency for clock: 13.03 MHz (PASS @ 12Mhz)
```

## Memory Map
| Peripheral | Start Address | End Address |
|------------|---------------|-------------|
| Bios       |  0x00000000   | 0x000000FF  |
| Led        |  0x00000100   | 0x00000100  |

## Peripherals
### Led
| Byte 3   | Byte 2   | Byte 1           | Byte 0        |
|----------|----------|------------------|---------------|
| reserved | reserved | Register Address | Register Data |

See [Appendix D of the iCE40 LED Driver Usage Guide](https://github.com/standardsemiconductor/VELDT-info/blob/master/ICE40LEDDriverUsageGuide.pdf) for information on LED control bus addressable registers and register field descriptions.