# Stak Scheme

[![GitHub Action](https://img.shields.io/github/actions/workflow/status/raviqqe/stak/test.yaml?branch=main&style=flat-square)](https://github.com/raviqqe/stak/actions)
[![Crate](https://img.shields.io/crates/v/stak.svg?style=flat-square)](https://crates.io/crates/stak)
[![Codecov](https://img.shields.io/codecov/c/github/raviqqe/stak.svg?style=flat-square)](https://codecov.io/gh/raviqqe/stak)
[![License](https://img.shields.io/github/license/raviqqe/stak.svg?style=flat-square)](https://github.com/raviqqe/stak/blob/main/LICENSE)

The miniature, embeddable R7RS Scheme implementation in Rust

Stak Scheme aims to be:

- An embeddable Scheme interpreter for Rust with very small memory footprint and reasonable performance
- The minimal implementation of [the R7RS-small standard][r7rs-small]
  - A subset of [Chibi Scheme](https://github.com/ashinn/chibi-scheme), [Gauche](https://github.com/shirok/Gauche), and [Guile](https://www.gnu.org/software/guile/)
- A portable scripting environment that supports even no-`std` and no-`alloc` platforms

For more information and usage, visit [the full documentation](https://raviqqe.github.io/stak/install).

## Install

### Libraries

To install Stak Scheme as a library in your Rust project, run:

```sh
cargo add stak
cargo add --build stak-build
```

For full examples, see [the `examples` directory](https://github.com/raviqqe/stak/tree/main/examples).

### Command line tools

To install the Scheme interpreter as a command, run:

```sh
cargo install stak
```

## Examples

### Embedding Scheme scripts in Rust

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

Now, you can include the Scheme script into a program in Rust using [the `stak::include_module` macro](https://docs.rs/stak/latest/stak/macro.include_module.html).

```rust
use core::error::Error;
use stak::{
    device::StdioDevice,
    file::VoidFileSystem,
    include_module,
    process_context::VoidProcessContext,
    module::{Module, UniversalModule},
    r7rs::{SmallError, SmallPrimitiveSet},
    time::VoidClock,
    vm::Vm,
};

const HEAP_SIZE: usize = 1 << 16;

// Include a Scheme script in the bytecode format built by the build script above.
static MODULE: UniversalModule = include_module!("hello.scm");

fn main() -> Result<(), Box<dyn Error>> {
    run(&MODULE.bytecode())?;

    Ok(())
}

fn run(bytecodes: &[u8]) -> Result<(), SmallError> {
    // Prepare a heap memory of a virtual machine.
    let mut heap = [Default::default(); HEAP_SIZE];
    // Create a virtual machine with its heap memory primitive procedures.
    let mut vm = Vm::new(
        &mut heap,
        SmallPrimitiveSet::new(
            // Attach standard input, output, and error of this process to a virtual machine.
            StdioDevice::new(),
            // Use void system interfaces for security because we don't need them for this example.
            VoidFileSystem::new(),
            VoidProcessContext::new(),
            VoidClock::new(),
        ),
    )?;

    // Initialize a virtual machine with bytecodes.
    vm.initialize(bytecodes.iter().copied())?;
    // Run bytecodes on a virtual machine.
    vm.run()
}
```

### Communication between Scheme and Rust

Currently, in-memory standard input (`stdin`) and output (`stdout`) to Scheme scripts are the only way to communicate information between Rust programs and Scheme scripts.

```rust
use core::{error::Error, str::{self, FromStr}};
use stak::{
    device::ReadWriteDevice,
    file::VoidFileSystem,
    include_module,
    process_context::VoidProcessContext,
    module::{Module, UniversalModule},
    r7rs::{SmallError, SmallPrimitiveSet},
    time::VoidClock,
    vm::Vm,
};

const BUFFER_SIZE: usize = 1 << 8;
const HEAP_SIZE: usize = 1 << 16;

static MODULE: UniversalModule = include_module!("fibonacci.scm");

fn main() -> Result<(), Box<dyn Error>> {
    let input = 15;
    let mut output = vec![];
    let mut error = vec![];

    run(&MODULE.bytecode(), input.to_string().as_bytes(), &mut output, &mut error)?;

    // If stderr is not empty, we assume that some error has occurred.
    if !error.is_empty() {
        return Err(str::from_utf8(&error)?.into());
    }

    // Decode and test the output.
    assert_eq!(isize::from_str(&str::from_utf8(&output)?)?, 610);

    Ok(())
}

fn run(
    bytecodes: &[u8],
    input: &[u8],
    output: &mut Vec<u8>,
    error: &mut Vec<u8>,
) -> Result<(), SmallError> {
    let mut heap = [Default::default(); HEAP_SIZE];
    let mut vm = Vm::new(
        &mut heap,
        SmallPrimitiveSet::new(
            // Create and attach an in-memory I/O device.
            ReadWriteDevice::new(input, output, error),
            VoidFileSystem::new(),
            VoidProcessContext::new(),
            VoidClock::new(),
        ),
    )?;

    vm.initialize(bytecodes.iter().copied())?;
    vm.run()
}
```

## References

- This project is based on [Ribbit Scheme][ribbit], the small and portable R4RS implementation.
- [Scheme programming language][scheme]
- [The R7RS-small standard][r7rs-small]

## License

[MIT](https://github.com/raviqqe/stak/blob/main/LICENSE)

[scheme]: https://www.scheme.org/
[r7rs-small]: https://small.r7rs.org/
[ribbit]: https://github.com/udem-dlteam/ribbit
