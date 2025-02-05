---
title: Running on no-std and no-alloc environment in Rust
description: How to enable and disable std and alloc features of Stak Scheme for specific environments in Rust
---

This page explains how to enable or disable `std` and `alloc` features of Stak Scheme for specific environments in Rust. By reading this page, you will learn:

- How to disable `std` and `alloc` features for the Stak Scheme library in Rust crates.

## Installing the Stak Scheme library without `std` and `alloc` features

To disable `std` and `alloc` features for the Stak Scheme library, you need to disable its default features first in your crate's `Cargo.toml` file. This is because a Stak Scheme library enables the features by default. In the `features` field of the dependency entry, list up all the features you need.

For a full list of features available, see [the Rust documentation of the `stak` crate](https://docs.rs/stak).

```toml
stak = { version = "0.7.0", default-features = false, features = [
  # List all the features you need.
  "float",
] }
```

## Running Scheme virtual machines

# References

- [`examples/embedded-script` directory on GitHub](https://github.com/raviqqe/stak/tree/main/examples/embedded-script)
