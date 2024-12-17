# Stak Scheme

[![GitHub Action](https://img.shields.io/github/actions/workflow/status/raviqqe/stak/test.yaml?branch=main&style=flat-square)](https://github.com/raviqqe/stak/actions)
[![Crate](https://img.shields.io/crates/v/stak.svg?style=flat-square)](https://crates.io/crates/stak)
[![Codecov](https://img.shields.io/codecov/c/github/raviqqe/stak.svg?style=flat-square)](https://codecov.io/gh/raviqqe/stak)
[![License](https://img.shields.io/github/license/raviqqe/stak.svg?style=flat-square)](https://github.com/raviqqe/stak/blob/main/LICENSE)

The no-`std` and no-`alloc` R7RS Scheme implementation in Rust

The full documentation is [here](https://raviqqe.github.io/stak).

## Install

### Library

```sh
cargo add stak
```

### Command line tools

```sh
# The Scheme interpreter
cargo install stak

# The minimal Scheme interpreter (5 times smaller!)
cargo install mstak

# The Scheme to bytecode compiler
cargo install stak-compile

# The bytecode interpreter
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
