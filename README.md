# Stak Scheme

[![GitHub Action](https://img.shields.io/github/actions/workflow/status/raviqqe/stak/test.yaml?branch=main&style=flat-square)](https://github.com/raviqqe/stak/actions)
[![Crate](https://img.shields.io/crates/v/stak.svg?style=flat-square)](https://crates.io/crates/stak)
[![Codecov](https://img.shields.io/codecov/c/github/raviqqe/stak.svg?style=flat-square)](https://codecov.io/gh/raviqqe/stak)
[![License](https://img.shields.io/github/license/raviqqe/stak.svg?style=flat-square)](https://github.com/raviqqe/stak/blob/main/LICENSE)

The no-`std` and no-`alloc` R7RS Scheme implementation in Rust

The full documentation is [here](https://raviqqe.github.io/stak).

## Install

### Library

To install Stak Scheme as a library in your Rust project, run:

```sh
cargo add stak
```

### Command line tools

To install the Scheme interpreter and alike as command line tools, run:

```sh
# Install the Scheme interpreter.
cargo install stak

# Install the minimal Scheme interpreter (6 times smaller!)
cargo install mstak

# Install the Scheme to bytecode compiler and bytecode interpreter.
cargo install stak-compile
cargo install stak-interpret
```

## Examples

### Running a Scheme script

```rust
use core::error::Error;
use stak::{
    build::include_bytecode,
    device::StdioDevice,
    file::VoidFileSystem,
    process_context::VoidProcessContext,
    r7rs::{SmallError, SmallPrimitiveSet},
    time::VoidClock,
    vm::Vm,
};

const HEAP_SIZE: usize = 1 << 16;
const BYTECODES: &[u8] = include_bytecode!("hello.scm");

fn main() -> Result<(), Box<dyn Error>> {
    run(BYTECODES)?;

    Ok(())
}

fn run(bytecodes: &[u8]) -> Result<(), SmallError> {
    let mut heap = [Default::default(); HEAP_SIZE];
    let mut vm = Vm::new(
        &mut heap,
        SmallPrimitiveSet::new(
            StdioDevice::new(),
            VoidFileSystem::new(),
            VoidProcessContext::new(),
            VoidClock::new(),
        ),
    )?;

    vm.initialize(bytecodes.iter().copied())?;
    vm.run()
}
```

## License

[MIT](https://github.com/raviqqe/stak/blob/main/LICENSE)
