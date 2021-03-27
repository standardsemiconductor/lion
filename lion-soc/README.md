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
4. `cabal run com` to open serial port, then press any key to display the Lion SoC name. The Lion Soc will echo any further input. Press <kbd>Ctrl-c</kbd> to end the program. The default port for `com` is `/dev/ttyUSB0`. If you require a different port, specify it as an argument: `cabal run com -- path/to/port`. Other serial port programs will work as well; be sure to specify 8 data bits, 1 stop bit, no flow control, and a baud rate of 19200.
```
   __   _             ____     _____
  / /  (_)__  ___    / __/__  / ___/
 / /__/ / _ \/ _ \  _\ \/ _ \/ /__  
/____/_/\___/_//_/ /___/\___/\___/  

Standard Semiconductor (c) 2021

Checking FLASH...SUCCESS
```
To compile, synthesize, and route Lion SoC without programming: `cabal run soc`

### Clean
`cabal run soc -- clean`

## Metrics as of Mar 27 2021 for iCE40
### Device utilisation
```
Device utilisation:
   ICESTORM_LC:  2669/ 5280    50%
  ICESTORM_RAM:     8/   30    26%
         SB_IO:     6/   96     6%
         SB_GB:     8/    8   100%
  ICESTORM_DSP:     1/    8    12%
ICESTORM_HFOSC:     1/    1   100%
        SB_SPI:     1/    2    50%
   SB_LEDDA_IP:     1/    1   100%
   SB_RGBA_DRV:     1/    1   100%
ICESTORM_SPRAM:     4/    4   100%
```
### Clock frequency
```
Max frequency for clock: 13.89 MHz (PASS @ 12Mhz)
```

## Memory Map
| Peripheral | Start Address | End Address |
|------------|---------------|-------------|
| LED        |  0x00000000   | 0x00000000  |
| UART       |  0x00000004   | 0x00000004  |
| SPI Flash  |  0x00000008   | 0x00000008  |
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

#### UART Usage Examples
##### UART Write Byte
```assembly
   li   a0, 0xAB    # load desired byte to send
   li   a1, 0x4     # set pointer to UART peripheral memory location
1: lbu  a2, 0x2(a1) # read status 
   andi a2, a2, 0x1 # mask transmitter status
   bnez a2, 1b      # wait until transmitter empty
   sb   a0, (a1)    # transmit byte
```

##### UART Read Byte
```assembly
   li   a0, 0x4     # set pointer to UART peripheral memory location
1: lbu  a1, 0x2(a0) # read status register
   andi a1, a1, 0x2 # mask receiver status
   beqz a1, 1b      # wait until receiver full
   lbu  a1, 0x1(a0) # read receiver buffer
```
### SPI Flash

See [VELDT-info](https://github.com/standardsemiconductor/VELDT-info#veldt-info) for more information about the on-board [SPI flash chip](https://github.com/standardsemiconductor/VELDT-info/blob/master/AT25SF081.pdf) and the [Lattice SPI and SysBus interface](https://github.com/standardsemiconductor/VELDT-info/blob/master/AdvancediCE40SPII2CHardenedIPUsageGuide.pdf).

#### Peripheral Register
| Byte 3        | Byte 2                                | Byte 1                 | Byte 0              |
|---------------|---------------------------------------|------------------------|---------------------|
| SysBus Status | SysBus Read/Write AND SysBus Received | SysBus Command Address | SysBus Command Data |

##### SysBus Status
```
76543210
.......*
       |__ 0 = Idle, 1 = Busy
```

##### SysBus Read/Write AND SysBus Received
When writing to this memory location: 0 indicates READ, 1 indicates WRITE.

When reading from this memory location: most recently received byte from SysBus.

SysBus Read/Write:
```
76543210
.......*
       |__ 0 = READ, 1 = WRITE
```

#### SPI Usage Examples

##### SPI Enable
```assembly
   li   a0, 0x8        # set pointer to SPI peripheral memory address
1: lbu  a1, 3(a0)      # read bus status
   bnez a1, 1b         # wait until bus idle
   li   a1, 0x00010000 # set WRITE mode
   li   a2, 0x00000900 # set CR1 address
   or   a1, a1, a2     # set CR1 address
   ori  a1, a1, 0x80   # set SPI enable
   sw   a1, (a0)       # send SPI enable command
```
##### SPI Read Status Register
```assembly
   li   a0, 0x8        # set pointer to SPI peripheral memory address
1: lbu  a1, 3(a0)      # read bus status
   bnez a1, 1b         # wait until bus idle
   li   a1, 0x00000C00 # set READ mode and SR address
   sw   a1, (a0)       # send SPI read status command
2: lbu  a1, 3(a0)      # read bus status
   bnez a1, 2b         # wait until bus idle
   lbu  a1, 2(a0)      # read SPI status from bus received
```