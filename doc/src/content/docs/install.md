---
title: Install
description: How to install Stak Scheme as libraries and command line tools
---

This page explains how to install Stak Scheme for your projects. By reading this page, you would learn:

- How to install Stak Scheme as a library in Rust projects.
- How to install Stak Scheme as command line tools.

### Library

To install Stak Scheme as a library in your Rust project, run:

```sh
cargo add stak stak-build
```

The two crates have different roles:

- The `stak` crate provides virtual machines and utilities to run Scheme programs in Rust.
- The `stak-build` crate builds Scheme scripts in `build.rs` build scripts.

### Command line tools

#### Interpreters

To install the Scheme interpreter, run:

```sh
cargo install stak
```

#### Development tools

To install the other development tools of Stak Scheme, run:

```sh
cargo install stak-compile stak-interpret
```

The `stak-compile` command is a Scheme-to-bytecode compiler. The `stak-interpret` command is a bytecode compiler that intakes bytecode files built by the `stak-compile` command.