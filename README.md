# Where Lions Roam: RISC-V on the VELDT

Lion is a formally verified 5-stage pipelined RISC-V processor core written in Haskell. Lion targets the VELDT FPGA development board.

This repository contains three parts:
  1. The Lion library: a pipelined RISC-V core.
  2. The Lion formal verification executable: verify the core using riscv-formal.
  3. The Lion SoC: a System-on-Chip demonstrating usage of the Lion core.

## Lion library
### Usage:
1. Add `lion` to build depends section of Cabal file
2. import module in source files `import Lion.Core`

Building from source:

## Lion formal verification

## Lion SoC


