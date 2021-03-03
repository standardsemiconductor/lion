# Where Lions Roam: RISC-V on the VELDT

[![Haskell CI](https://github.com/standardsemiconductor/lion/actions/workflows/haskell.yml/badge.svg?branch=main)](https://github.com/standardsemiconductor/lion/actions/workflows/haskell.yml)
[![Hackage][hackage-badge]][hackage]
[![Hackage Dependencies][hackage-deps-badge]][hackage-deps]

Lion is a formally verified, 5-stage pipeline [RISC-V](https://riscv.org) core. Lion targets the [VELDT FPGA development board](https://standardsemiconductor.com) and is written in Haskell using [Clash](https://clash-lang.org).

This repository contains four parts:
  1. The Lion library: a pipelined RISC-V core.
  2. [lion-formal](https://github.com/standardsemiconductor/lion/tree/main/lion-formal): formally verify the core using [riscv-formal](https://github.com/standardsemiconductor/riscv-formal/tree/lion).
  3. [lion-soc](https://github.com/standardsemiconductor/lion/tree/main/lion-soc): a System-on-Chip demonstrating usage of the Lion core on the VELDT.
  4. [lion-metric](https://github.com/standardsemiconductor/lion/tree/main/lion-metric): Observe Yosys synthesis metrics on the Lion Core.

## Lion library
### Usage:
1. Add `lion` to build depends section of Cabal file
2. import module in source files `import Lion.Core`

When connecting the `core` to memory and peripherals, ensure single cycle latency.

## Features
### Current Support
Architecture: RV32I (no FENCE, ECALL, EBREAK)

### Future Support 
**All features will be added in a configurable manner extending the base RV32I configuration noted above**
* Zicsr, Control and Status Register (CSR) Instructions
* CSR registers
* RV32IM
* Hard IP ALU

[hackage]:            <https://hackage.haskell.org/package/lion>
[hackage-badge]:      <https://img.shields.io/hackage/v/lion.svg?color=success>
[hackage-deps-badge]: <https://img.shields.io/hackage-deps/v/lion.svg>
[hackage-deps]:       <http://packdeps.haskellers.com/feed?needle=lion>