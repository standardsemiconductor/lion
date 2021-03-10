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

## Metrics as of Mar 9 2021 for iCE40
### Device utilisation
```
Device utilisation:
   ICESTORM_LC:  2383/ 5280    45%
  ICESTORM_RAM:     8/   30    26%
         SB_GB:     5/    8    62%
ICESTORM_HFOSC:     1/    1   100%
   SB_LEDDA_IP:     1/    1   100%
   SB_RGBA_DRV:     1/    1   100%
```
### Clock frequency
```
Max frequency for clock: 13.68 MHz (PASS @ 12Mhz)
```

## Memory Map
| Peripheral | Start Address | End Address |
|------------|---------------|-------------|
| Led        |  0x00000000   | 0x00000000  |
| Uart       |  0x00000004   | 0x00000004  |
| Rom        |  0x00000400   | 0x000007FF  |

## Peripherals
### Led
| Byte 3   | Byte 2   | Byte 1           | Byte 0        |
|----------|----------|------------------|---------------|
| reserved | reserved | Register Address | Register Data |

See [Appendix D of the iCE40 LED Driver Usage Guide](https://github.com/standardsemiconductor/VELDT-info/blob/master/ICE40LEDDriverUsageGuide.pdf) for information on LED control bus addressable registers and register field descriptions.