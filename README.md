# Stak

[![GitHub Action](https://img.shields.io/github/actions/workflow/status/raviqqe/stak/test.yaml?branch=main&style=flat-square)](https://github.com/raviqqe/stak/actions)
[![Crate](https://img.shields.io/crates/v/stak.svg?style=flat-square)](https://crates.io/crates/stak)
[![Codecov](https://img.shields.io/codecov/c/github/raviqqe/stak.svg?style=flat-square)](https://codecov.io/gh/raviqqe/stak)
[![License](https://img.shields.io/github/license/raviqqe/stak.svg?style=flat-square)](LICENSE)

No-`std` and no-`alloc` Scheme implementation in Rust

For working examples, see [a `features` directory](/features).

## Goal

- Minimal implementation of [R7RS small][r7rs-small]
- Subset of [Chibi Scheme](https://github.com/ashinn/chibi-scheme), [Gauche](https://github.com/shirok/Gauche), and [Guile](https://www.gnu.org/software/guile/)
- Small memory footprint

## Limitations

- Numbers are 63-bit integers.
- Some runtime errors are not raised.
  - e.g. argument count and out-of-memory errors

## References

- [R7RS small][r7rs-small]
- [Ribbit Scheme](https://github.com/udem-dlteam/ribbit)

## License

[MIT](LICENSE)

[r7rs-small]: https://small.r7rs.org/
