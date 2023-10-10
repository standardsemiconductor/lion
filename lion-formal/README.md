# Lion Formal Verification

## Setup
At minimum to run Lion formal verification using [riscv-formal](https://github.com/standardsemiconductor/riscv-formal):
> You'll need Yosys, SymbiYosys, and Boolector for the formal proofs.
> See [here](https://symbiyosys.readthedocs.io/en/latest/install.html)
> for intall instructions.

The [nightly pipeline](https://github.com/standardsemiconductor/lion/blob/main/.github/workflows/haskell.yml)
also has up-to-date steps to install Yosys, SymbiYosys,
and Boolector.

If you want to inspect counter example traces or disassemble the code in the
counter example traces see the [ricsv-formal prerequisites](https://github.com/standardsemiconductor/riscv-formal/blob/lion/docs/quickstart.md#prerequisites).

## Usage
Build:      `cabal build`

Run checks: `cabal run`

Clean:      `cabal run formal -- clean`

