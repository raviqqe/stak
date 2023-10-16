---
title: Miniature, embeddable Scheme
description: No-std and no-alloc Scheme implementation in Rust.
template: splash
hero:
  tagline: No-std and no-alloc Scheme implementation in Rust.
  actions:
    - text: Examples
      link: examples/continuation
      icon: right-arrow
      variant: primary
---

# Stak

[![GitHub Action](https://img.shields.io/github/actions/workflow/status/raviqqe/stak/test.yaml?branch=main&style=flat-square)](https://github.com/raviqqe/stak/actions)
[![Crate](https://img.shields.io/crates/v/stak.svg?style=flat-square)](https://crates.io/crates/stak)
[![Codecov](https://img.shields.io/codecov/c/github/raviqqe/stak.svg?style=flat-square)](https://codecov.io/gh/raviqqe/stak)
[![License](https://img.shields.io/github/license/raviqqe/stak.svg?style=flat-square)](LICENSE)

No-`std` and no-`alloc` [Scheme][scheme] implementation in Rust

For working examples, see [the documentation](https://raviqqe.github.io/stak).

## Goal

- Minimal implementation of [R7RS small][r7rs-small]
- Subset of [Chibi Scheme](https://github.com/ashinn/chibi-scheme), [Gauche](https://github.com/shirok/Gauche), and [Guile](https://www.gnu.org/software/guile/)
- Small memory footprint

## Limitations

- Numbers are 63-bit integers.
- Some runtime errors are not raised.
  - e.g. argument count and out-of-memory errors

## References

- [Ribbit Scheme][ribbit]
- [R7RS small][r7rs-small]
- [Scheme][scheme]

## License

[MIT](https://github.com/raviqqe/stak/blob/main/LICENSE)

[scheme]: https://www.scheme.org/
[r7rs-small]: https://small.r7rs.org/
[ribbit]: https://github.com/udem-dlteam/ribbit
