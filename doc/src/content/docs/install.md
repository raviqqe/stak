---
title: Install
description: How to install Stak Scheme as libraries and command line tools
---

This page explains how to install Stak Scheme for your projects. By reading this page, you will learn:

- How to install Stak Scheme as a library in Rust projects.
- How to install Stak Scheme as command line tools.

### Libraries

To install Stak Scheme as a library in your Rust project, run the following commands in your terminal:

```sh
cargo add stak
cargo add --build stak-build
cargo install stak-compile
```

The two crates have different roles:

- The `stak` crate provides virtual machines and utilities to run Scheme programs in Rust.
- The `stak-build` crate builds Scheme scripts in `build.rs` build scripts.
- The `stak-compile` crate is a Stak Scheme compiler to compile Scheme scripts into bytecodes.

### Command line tools

#### Interpreters

To install the Scheme interpreter, run the following command in your terminal:

```sh
cargo install stak
```

#### Development tools

To install the other development tools of Stak Scheme, run the following command in your terminal:

```sh
cargo install stak-compile stak-interpret
```

The `stak-compile` command is a Scheme-to-bytecode compiler. The `stak-interpret` command is a bytecode interpreter that intakes bytecode files built by the `stak-compile` command.
