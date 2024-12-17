# Stak Scheme

[![GitHub Action](https://img.shields.io/github/actions/workflow/status/raviqqe/stak/test.yaml?branch=main&style=flat-square)](https://github.com/raviqqe/stak/actions)
[![Crate](https://img.shields.io/crates/v/stak.svg?style=flat-square)](https://crates.io/crates/stak)
[![Codecov](https://img.shields.io/codecov/c/github/raviqqe/stak.svg?style=flat-square)](https://codecov.io/gh/raviqqe/stak)
[![License](https://img.shields.io/github/license/raviqqe/stak.svg?style=flat-square)](https://github.com/raviqqe/stak/blob/main/LICENSE)

The no-`std` and no-`alloc` R7RS Scheme implementation in Rust

The full documentation is [here](https://raviqqe.github.io/stak).

## Install

### Library

```sh
cargo add stak
```

### Command line tools

```sh
# The Scheme interpreter
cargo install stak

# The minimal Scheme interpreter (5 times smaller!)
cargo install mstak

# The Scheme to bytecode compiler
cargo install stak-compile

# The bytecode interpreter
cargo install stak-interpret
```

## Examples

## License

[MIT](https://github.com/raviqqe/stak/blob/main/LICENSE)
