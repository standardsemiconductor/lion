# Lion SoC

System-On-Chip using Lion and targeting the [VELDT FPGA development board](https://standardsemiconductor.com).

## Prerequisites
* [Project IceStorm](https://github.com/standardsemiconductor/VELDT-info#project-icestorm)
* [riscv-gnu-toolchain](https://github.com/riscv/riscv-gnu-toolchain)
  * Need `riscv64-unknown-*` binaries

## Usage
1. Ensure the VELDT is ON and in the FLASH mode.
2. `cabal run soc -- prog` 
3. Cycle power switch, set mode switch to FPGA.
4. `cabal run com` to open serial port, then press any key to display the Lion SoC name. The Lion Soc will echo any further input. Press <kbd>Ctrl-c</kbd> to end the program. The default port for `com` is `/dev/ttyUSB0`. If you require a different port, specify it as an argument: `cabal run com -- path/to/port`. Other serial port programs will work as well; be sure to specify a 8 data bits, 1 stop bit, no flow control, and a baud rate of 19200.
```
   __   _             ____     _____
  / /  (_)__  ___    / __/__  / ___/
 / /__/ / _ \/ _ \  _\ \/ _ \/ /__  
/____/_/\___/_//_/ /___/\___/\___/  

Standard Semiconductor (c) 2021
```
To compile, synthesize, and route Lion SoC without programming: `cabal run soc`

### Clean
`cabal run soc -- clean`

## Metrics as of Mar 23 2021 for iCE40
### Device utilisation
```
Device utilisation:
   ICESTORM_LC:  2691/ 5280    50%
  ICESTORM_RAM:     8/   30    26%
         SB_IO:     2/   96     2%
         SB_GB:     8/    8    62%
ICESTORM_HFOSC:     1/    1   100%
   SB_LEDDA_IP:     1/    1   100%
   SB_RGBA_DRV:     1/    1   100%
ICESTORM_SPRAM:     4/    4   100%
```
### Clock frequency
```
Max frequency for clock: 14.98 MHz (PASS @ 12Mhz)
```

## Memory Map
| Peripheral | Start Address | End Address |
|------------|---------------|-------------|
| LED        |  0x00000000   | 0x00000000  |
| UART       |  0x00000004   | 0x00000004  |
| ROM        |  0x00000400   | 0x000007FF  |
| SPRAM      |  0x00020000   | 0x0003FFFF  |

## Peripherals
### Led
| Byte 3   | Byte 2   | Byte 1           | Byte 0        |
|----------|----------|------------------|---------------|
| Reserved | Reserved | Register Address | Register Data |

See [Appendix D of the iCE40 LED Driver Usage Guide](https://github.com/standardsemiconductor/VELDT-info/blob/master/ICE40LEDDriverUsageGuide.pdf) for information on LED control bus addressable registers and register field descriptions.

### UART
| Byte 3   | Byte 2 | Byte 1    | Byte 0    |
|----------|--------|-----------|-----------|
| Reserved | Status | RX Buffer | TX Buffer | 

Status Byte:
```
76543210
......**
      ||__Transmitter Status: 0 = Empty (Idle), 1 = Full (Busy)
      |___Receiver Status:    0 = Empty, 1 = Full
```

Reading the RX Buffer resets the UART receiver.

Writing the TX Buffer resets the UART transmitter.