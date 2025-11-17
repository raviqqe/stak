# Stak Scheme

[![GitHub Action](https://img.shields.io/github/actions/workflow/status/raviqqe/stak/test.yaml?branch=main&style=flat-square)](https://github.com/raviqqe/stak/actions)
[![Crate](https://img.shields.io/crates/v/stak.svg?style=flat-square)](https://crates.io/crates/stak)
[![Codecov](https://img.shields.io/codecov/c/github/raviqqe/stak.svg?style=flat-square)](https://codecov.io/gh/raviqqe/stak)
[![CodSpeed](https://img.shields.io/endpoint?url=https://codspeed.io/badge.json&style=flat-square)](https://codspeed.io/raviqqe/stak)
[![License](https://img.shields.io/github/license/raviqqe/stak.svg?style=flat-square)](https://github.com/raviqqe/stak/blob/main/LICENSE)

The miniature, embeddable R7RS Scheme implementation in Rust

Stak Scheme aims to be:

- An embeddable Scheme interpreter for Rust with very small memory footprint and reasonable performance
  - Its virtual machine (VM) is written in only 1.5 KLOC in Rust.
- The minimal implementation of [the R7RS-small standard][r7rs-small]
  - A subset of [Chibi Scheme](https://github.com/ashinn/chibi-scheme), [Gauche](https://github.com/shirok/Gauche), and [Guile](https://www.gnu.org/software/guile/)
- A portable scripting environment that supports even no-`std` and no-`alloc` platforms

For more information and usage, visit [the full documentation](https://raviqqe.com/stak/install).

## Install

### Interpreter

To install the Scheme interpreter as a command, run:

```sh
cargo install stak
cargo install stak-repl
```

### Libraries

To install Stak Scheme as a library in your Rust project, run:

```sh
cargo add stak
cargo add --build stak-build
cargo install stak-compile
```

For examples, see [the `examples` directory](https://github.com/raviqqe/stak/tree/main/examples).

## Performance

### Computational benchmarks

The Stak Scheme interpreter runs 1.6 to 2.3 times slower than Python 3 at computationally heavy tasks depending on its configuration and benchmarks. For all the benchmark results, see [the GitHub Action](https://github.com/raviqqe/stak/actions/workflows/bench.yaml).

- Baseline: Python 3.13
- Environment: Ubuntu 24.04, x86-64

| Benchmark        | Stak (minimal [^1]) | Stak (full [^2]) |
| ---------------- | ------------------: | ---------------: |
| Fibonacci number |        1.80x slower |     1.98x slower |
| Integer sum      |        1.61x slower |     1.87x slower |
| Tak function     |        2.10x slower |     2.27x slower |

### Startup benchmarks

Although Stak Scheme's minimality comes at the cost of speed, it is very fast at startup.

This means that Stak Scheme is suitable for embedding many small pieces of Scheme programs in Rust due to its tiny overhead on program initialization.

- Environment: Ubuntu 24.04, x86-64

| Benchmark        | Stak (full [^2]) | Lua 5.4 |
| ---------------- | ---------------: | ------: |
| Empty program    |         0.534 us | 48.9 us |
| Integer addition |          22.9 us | 50.0 us |

[^1]: Minimal: Integer-only support + standard libraries based on libc

[^2]: Full: 64-bit floating-point number support + standard libraries based on the `std` library in Rust

## References

- This project is based on [Ribbit Scheme][ribbit], the small and portable R4RS implementation.
- [Scheme programming language][scheme]
- [The R7RS-small standard][r7rs-small]

## License

[MIT](https://github.com/raviqqe/stak/blob/main/LICENSE)

[scheme]: https://www.scheme.org/
[r7rs-small]: https://small.r7rs.org/
[ribbit]: https://github.com/udem-dlteam/ribbit
