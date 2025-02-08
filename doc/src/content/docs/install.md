---
title: Install
description: How to install Stak Scheme as libraries and command line tools
---

This page explains how to install Stak Scheme as command line tools or libraries. By reading this page, you will learn:

- How to install Stak Scheme as command line tools.
- How to install Stak Scheme as libraries in Rust crates.

## Command line tools

### Interpreters

To install the Stak Scheme interpreter, run the following command in your terminal.

```sh
cargo install stak
```

Now, you can run programs written in [R7RS Scheme][r7rs] with the `stak` command as follows.

```sh
stak hello.scm
```

The content of the `hello.scm` would look like:

```sh
(import (scheme base))

(write-string "Hello, world!\n")
```

### Development tools

To install development tools of Stak Scheme, run the following command in your terminal:

```sh
cargo install stak-compile stak-interpret
```

The `stak-compile` command is a Scheme-to-bytecode compiler. The `stak-interpret` command is a bytecode interpreter that ingests and runs bytecode files built by the `stak-compile` command.

## Libraries

To install Stak Scheme as a library in your Rust crate, run the following commands in your terminal.

```sh
cargo add stak
cargo add --build stak-build
cargo install stak-compile
```

The crates have different roles:

- The `stak` crate provides virtual machines and utilities to run Scheme programs in Rust.
- The `stak-build` crate builds Scheme scripts in `build.rs` build scripts.
- The `stak-compile` crate is a Stak Scheme compiler to compile Scheme scripts into bytecodes.

[r7rs]: https://r7rs.org/
