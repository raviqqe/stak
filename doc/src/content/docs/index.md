---
title: Miniature, embeddable Scheme
description: The no-std and no-alloc Scheme implementation in Rust.
template: splash
hero:
  image:
    alt: Icon
    file: ../../../public/icon.svg
  tagline: The no-std and no-alloc Scheme implementation in Rust.
  actions:
    - text: Rust integration
      link: rust/stak
      icon: document
      variant: primary
    - text: Examples
      link: examples/continuation
      icon: right-arrow
      variant: secondary
---

# Stak Scheme

Stak Scheme is a miniature, embeddable [Scheme][scheme] implementation in Rust.

## Goals

- Minimal implementation of [R7RS small][r7rs-small]
- Subset of [Chibi Scheme](https://github.com/ashinn/chibi-scheme), [Gauche](https://github.com/shirok/Gauche), and [Guile](https://www.gnu.org/software/guile/)
- Small memory footprint

## Limitations

- Number representation is either [a 64-bit floating point number (IEEE 754)](https://en.wikipedia.org/wiki/IEEE_754) or a 63-bit integer.
- Only ASCII characters are supported in strings.
- Certain runtime errors are not raised as exceptions.
  - e.g. argument count mismatch on procedure calls and out-of-memory errors

## References

- This project is based on [Ribbit Scheme][ribbit], the small and portable R4RS implementation.
- [Scheme][scheme]
- [The R7RS-small standard][r7rs-small]

## License

[MIT](https://github.com/raviqqe/stak/blob/main/LICENSE)

[scheme]: https://www.scheme.org/
[r7rs-small]: https://small.r7rs.org/
[ribbit]: https://github.com/udem-dlteam/ribbit
