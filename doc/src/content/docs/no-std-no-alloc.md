---
title: Running in no-std and no-alloc environment in Rust
description: How to enable and disable std and alloc features of Stak Scheme for specific environments in Rust
---

This page explains how to use Stak Scheme for dynamic scripting with neither `std` nor `alloc` crates in Rust. By reading this page, you will learn:

- How to disable `std` and `alloc` features for the Stak Scheme library in Rust crates.
- How to run the Stak Scheme virtual machines without `std` and `alloc` crates in Rust.

The full source codes used in this page is available at [the example crate in the Stak Scheme repository][source].

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

First, declare that your crate does not use `std` or `alloc` crates in `lib.rs`.

```rust
// At the top of your `lib.rs` file.
#![no_std]
```

Then, import data structures that we initialize virtual machines of Stak Scheme with from the `stak` crate.

```rust
use stak::{
    // In-memory fixed buffer I/O device.
    device::FixedBufferDevice,
    // "Void" data structures with disabled operations.
    file::VoidFileSystem,
    process_context::VoidProcessContext,
    time::VoidClock,
};
```

Finally, you initialize and run a virtual machine of Stak Scheme configuring the [R7RS-small][r7rs-small] primitive set with the imported data structures. You use the in-memory I/O device for communication between Rust and Scheme, and "void" file system, process context, and clock which do not run any real operations to disable all the other primitives.

```rust
use stak::{r7rs::{SmallError, SmallPrimitiveSet}, vm::Vm};

const BUFFER_SIZE: usize = 1 << 8;
const HEAP_SIZE: usize = 1 << 16;

fn run_vm(
    bytecodes: &[u8],
    device: &mut FixedBufferDevice<BUFFER_SIZE, BUFFER_SIZE>,
) -> Result<(), SmallError> {
    let mut heap = [Default::default(); HEAP_SIZE];
    let mut vm = Vm::new(
        &mut heap,
        // Initialize the R7RS-small primitive set.
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

## References

- [`examples/no-std-no-alloc` directory on GitHub][source]

[r7rs-small]: https://small.r7rs.org/
[source]: https://github.com/raviqqe/stak/tree/main/examples/no-std-no-alloc
