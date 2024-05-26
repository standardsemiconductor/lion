# Where Lions Roam: RISC-V on the VELDT

[![Haskell CI](https://github.com/standardsemiconductor/lion/actions/workflows/haskell.yml/badge.svg?branch=main)](https://github.com/standardsemiconductor/lion/actions/workflows/haskell.yml)
[![Hackage][hackage-badge]][hackage]
[![Hackage Dependencies][hackage-deps-badge]][hackage-deps]

Lion is a formally verified, 5-stage pipeline [RISC-V](https://riscv.org) core. Lion targets the [VELDT FPGA development board](https://standardsemiconductor.com) and is written in Haskell using [Clash](https://clash-lang.org).

This repository contains four parts:
  1. The Lion library: a pipelined RISC-V core.
  2. [lion-formal](lion-formal): formally verify the core using [riscv-formal](https://github.com/standardsemiconductor/riscv-formal/tree/lion).
  3. [lion-soc](lion-soc): a System-on-Chip demonstrating usage of the Lion core on the VELDT.
  4. [lion-metric](lion-metric): Observe Yosys synthesis metrics on the Lion Core.

## Lion library
### Usage:
1. Add `lion` to build depends section of Cabal file
2. import module in source files `import Lion.Core`

When connecting the `core` to memory and peripherals, ensure single cycle latency.

## Clone the repository
1. `git clone https://github.com/standardsemiconductor/lion.git`
2. `cd lion`
3. `git submodule update --init`

## Features
### Current Support
* Architecture: RV32I (no FENCE, ECALL, EBREAK)
* Configurable ALU adder and subtractor: use a generic (+) and (-) or SB_MAC16 hard IP

### Future Support 
**All features will be added in a configurable manner extending the base RV32I configuration noted above**
* Zicsr, Control and Status Register (CSR) Instructions
* CSR registers
* RV32IM

Check out the [Lion Development project](https://github.com/standardsemiconductor/lion/projects/1) to see which features are in progress.

## References and Additional Resources
* [RISC-V Specifications](https://riscv.org/technical/specifications/)
* [Computer Architecture: A Quantitative Approach 6th Ed.](https://www.elsevier.com/books/computer-architecture/hennessy/978-0-12-811905-1)

[hackage]:            <https://hackage.haskell.org/package/lion>
[hackage-badge]:      <https://img.shields.io/hackage/v/lion.svg?color=success>
[hackage-deps-badge]: <https://img.shields.io/hackage-deps/v/lion.svg>
[hackage-deps]:       <http://packdeps.haskellers.com/feed?needle=lion>
