---
title: Running on no-std and no-alloc environment in Rust
description: How to enable and disable std and alloc features of Stak Scheme for specific environments in Rust
---

This page explains how to enable or disable `std` and `alloc` features of Stak Scheme for specific environments in Rust. By reading this page, you will learn:

- How to disable `std` and `alloc` features for the Stak Scheme library in Rust crates.

# Installing the Stak Scheme library without `std` and `alloc` features

To disable `std` and `alloc` features for the Stak Scheme library, you need to disable its default features first in your crate's `Cargo.toml` file. This is because the Stak Scheme library enables the features by default. In the `features` field of the dependency entry, list up all the features you need.

For a full list of features available, see [the Rust documentation of the `stak` crate](https://docs.rs/stak).

```toml
stak = { version = "0.7.0", default-features = false, features = [
  # List all the features you need.
  "float",
] }
```

# Running Scheme virtual machines

To run Scheme virtual machines without `std` and `alloc` features, you might initiailze the virtual machines with custom sets of primitive sets. In the following Rust program, you use an in-memory I/O devices for communication between Rust and Scheme. And, you disable all the other primitives using `Void` data structures that do not run any real operations.

```rust
use core::{
    error::Error,
    str::{self, FromStr},
};
use stak::{
    device::ReadWriteDevice,
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

pub fn run_script() -> Result<(), SmallError> {
    let input = 15;
    let mut output = vec![];
    let mut error = vec![];

    run_vm(
        &MODULE.bytecode(),
        input.to_string().as_bytes(),
        &mut output,
        &mut error,
    )?;

    // If stderr is not empty, we assume that some error has occurred.
    if !error.is_empty() {
        return Err(str::from_utf8(&error)?.into());
    }

    // Decode and test the output.
    assert_eq!(isize::from_str(&str::from_utf8(&output)?)?, 610);

    Ok(())
}

fn run_vm(
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
