---
title: Install
description: How to install Stak Scheme as command line tools or libraries
---

# Install

This page explains how to install Stak Scheme for your projects. By reading this page, you would learn:

- How to install Stak Scheme as a library in Rust projects
- How to install Stak Scheme as command line tools

### Library

To install Stak Scheme as a library in your Rust project, run:

```sh
cargo add stak stak-build
```

The crates have different roles:

- The `stak` crate provides virtual machines and utilities to run Scheme programs in Rust.
- The `stak-build` crate builds Scheme scripts in `build.rs` build scripts.

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
