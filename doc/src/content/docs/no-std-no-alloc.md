---
title: Running in no-std and no-alloc environment in Rust
description: How to enable and disable std and alloc features of Stak Scheme for specific environments in Rust
---

This page explains how to use Stak Scheme as a scripting environment in Rust with neither `std` nor `alloc` crates. By reading this page, you will learn:

- How to disable `std` and `alloc` features for the Stak Scheme library in Rust crates.
- How to run the Stak Scheme virtual machines without `std` and `alloc` crates in Rust.

## Installing the Stak Scheme library without `std` and `alloc` features

To disable `std` and `alloc` features of the Stak Scheme library, you need to disable its default features first in your crate's `Cargo.toml` file. This is because the Stak Scheme library enables these features by default. After that, list up all the other features you need in the `features` field of the dependency entry.

For a full list of features available, see [the Rust documentation of the `stak` crate](https://docs.rs/stak).

```toml
stak = { version = "SOME_VERSION", default-features = false, features = [
  # List all the features you need.
  "float",
] }
```

## Running virtual machines of Stak Scheme

To run virtual machines of Stak Scheme without `std` and `alloc` features, you would initialize them with custom sets of primitive sets.

In the following Rust program, you use an in-memory I/O device for communication between Rust and Scheme. And, you use "void" file system, clock, etc. that do not run any real operations to disable all the other primitives.

```rust
#![no_std]

use core::{
    num::ParseIntError,
    str::{self, FromStr, Utf8Error},
};
use stak::{
    device::FixedBufferDevice,
    file::VoidFileSystem,
    include_module,
    module::{Module, UniversalModule},
    process_context::VoidProcessContext,
    r7rs::{SmallError, SmallPrimitiveSet},
    time::VoidClock,
    vm::Vm,
};

const BUFFER_SIZE: usize = 1 << 8;
const HEAP_SIZE: usize = 1 << 16;

static MODULE: UniversalModule = include_module!("fibonacci.scm");

/// Calculates the Fibonacci number.
pub fn fibonacci(input: &str) -> Result<isize, MyError> {
    // Initialize an in-memory I/O device.
    let mut device = FixedBufferDevice::<BUFFER_SIZE, BUFFER_SIZE>::new(input.as_bytes());

    run_vm(&MODULE.bytecode(), &mut device)?;

    // If stderr is not empty, we assume that some error has occurred.
    if !device.error().is_empty() {
        return Err(MyError::InvalidInput);
    }

    // Decode the output.
    Ok(isize::from_str(&str::from_utf8(device.output())?)?)
}

fn run_vm(
    bytecodes: &[u8],
    device: &mut FixedBufferDevice<BUFFER_SIZE, BUFFER_SIZE>,
) -> Result<(), SmallError> {
    let mut heap = [Default::default(); HEAP_SIZE];
    let mut vm = Vm::new(
        &mut heap,
        SmallPrimitiveSet::new(
            // Attach an I/O device.
            device,
            // For the rest, you use "void" interfaces because they are not
            // needed.
            VoidFileSystem::new(),
            VoidProcessContext::new(),
            VoidClock::new(),
        ),
    )?;

    vm.initialize(bytecodes.iter().copied())?;
    vm.run()
}
```

For the full example, see [the example crate in the Stak Scheme repository](https://github.com/raviqqe/stak/blob/main/examples/no-std-no-alloc).

## References

- [`examples/no-std-no-alloc` directory on GitHub](https://github.com/raviqqe/stak/tree/main/examples/no-std-no-alloc)
