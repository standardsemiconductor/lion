# Where Lions Roam: RISC-V on the VELDT

Lion is a formally verified 5-stage pipelined [RISC-V](https://riscv.org) processor core written in Haskell using [Clash](https://clash-lang.org). Lion targets the [VELDT FPGA development board](https://standardsemiconductor.com).

This repository contains three parts:
  1. The Lion library: a pipelined RISC-V core.
  2. The Lion formal verification executable: verify the core using [riscv-formal](https://github.com/standardsemiconductor/riscv-formal/tree/lion).
  3. The Lion SoC: a System-on-Chip demonstrating usage of the Lion core.

## Lion library
### Usage:
1. Add `lion` to build depends section of Cabal file
2. import module in source files `import Lion.Core`

When connecting the `core` to memory and peripherals, ensure the read latency is 1 cycle.

## Lion formal verification
See the [lion-formal]() directory

## Lion SoC
See the [lion-soc]() directory

