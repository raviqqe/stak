# Stak Scheme

[![GitHub Action](https://img.shields.io/github/actions/workflow/status/raviqqe/stak/test.yaml?branch=main&style=flat-square)](https://github.com/raviqqe/stak/actions)
[![Crate](https://img.shields.io/crates/v/stak.svg?style=flat-square)](https://crates.io/crates/stak)
[![Codecov](https://img.shields.io/codecov/c/github/raviqqe/stak.svg?style=flat-square)](https://codecov.io/gh/raviqqe/stak)
[![License](https://img.shields.io/github/license/raviqqe/stak.svg?style=flat-square)](https://github.com/raviqqe/stak/blob/main/LICENSE)

The miniature, embeddable R7RS Scheme implementation in Rust

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

# Install the Scheme-to-bytecode compiler and bytecode interpreter.
cargo install stak-compile
cargo install stak-interpret
```

## Examples

### Embedding a Scheme script

First, prepare a Scheme script at `src/hello.scm`.

```scheme
(import (scheme base))

(write-string "Hello, world!\n")
```

Then, add a build script at `build.rs` to build the Scheme source file into bytecodes.

```rust no_run
use stak_build::{build_r7rs, BuildError};

fn main() -> Result<(), BuildError> {
    build_r7rs()
}
```

Now, you can include the Scheme script into a program in Rust using [the `stak::include_bytecode` macro](https://docs.rs/stak/latest/stak/macro.include_bytecode.html).

```rust
use core::error::Error;
use stak::{
    device::StdioDevice,
    file::VoidFileSystem,
    include_bytecode,
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

### Communication between Scheme and Rust

Currently, in-memory standard input (`stdin`) and output (`stdout`) to Scheme scripts are the only way to communicate information between Rust programs and Scheme scripts.

```rust
use core::{error::Error, ffi::CStr, str::FromStr};
use stak::{
    device::ReadWriteDevice,
    file::VoidFileSystem,
    include_bytecode,
    process_context::VoidProcessContext,
    r7rs::{SmallError, SmallPrimitiveSet},
    time::VoidClock,
    vm::Vm,
};

const BUFFER_SIZE: usize = 1 << 8;
const HEAP_SIZE: usize = 1 << 16;
const BYTECODES: &[u8] = include_bytecode!("fibonacci.scm");

fn main() -> Result<(), Box<dyn Error>> {
    let mut input = 24;
    let mut output = [0u8; BUFFER_SIZE];
    let mut error = [0u8; BUFFER_SIZE];

    run(BYTECODES, input.to_string().as_bytes(), &mut output, &mut error)?;

    let error = decode_buffer(&error)?;

    if !error.is_empty() {
        return Err(error.into());
    }

    println!("Answer: {}", isize::from_str(&decode_buffer(&output)?)?);

    Ok(())
}

fn run(
    bytecodes: &[u8],
    input: &[u8],
    output: &mut [u8],
    error: &mut [u8],
) -> Result<(), SmallError> {
    let mut heap = [Default::default(); HEAP_SIZE];
    let mut vm = Vm::new(
        &mut heap,
        SmallPrimitiveSet::new(
            ReadWriteDevice::new(input, output, error),
            VoidFileSystem::new(),
            VoidProcessContext::new(),
            VoidClock::new(),
        ),
    )?;

    vm.initialize(bytecodes.iter().copied())?;
    vm.run()
}

fn decode_buffer(buffer: &[u8]) -> Result<String, Box<dyn Error>> {
    Ok(CStr::from_bytes_until_nul(buffer)
        .map_err(|error| error.to_string())?
        .to_string_lossy()
        .into())
}
```

## License

[MIT](https://github.com/raviqqe/stak/blob/main/LICENSE)
