---
title: Performance
description: Limitations in the current implementation of Stak Scheme
---

This page describes the performance characteristics of the Stak Scheme interpreters.

## Computational benchmarks

The Stak Scheme interpreter runs 1.6 to 2.3 times slower than Python 3 at computationally heavy tasks depending on its configuration and benchmarks. For all the benchmark results, see [the GitHub Action](https://github.com/raviqqe/stak/actions/workflows/bench.yaml).

- Baseline: Python 3.13
- Environment: Ubuntu 24.04, x86-64

| Benchmark        | Stak (minimal [^1]) | Stak (full [^2]) |
| ---------------- | ------------------: | ---------------: |
| Fibonacci number |        1.80x slower |     1.98x slower |
| Integer sum      |        1.61x slower |     1.87x slower |
| Tak function     |        2.10x slower |     2.27x slower |

## Startup benchmarks

Although Stak Scheme's minimality comes at the cost of speed, it is very fast at startup.

This means that Stak Scheme is suitable for embedding many small pieces of Scheme programs in Rust due to its tiny overhead on program initialization.

- Environment: Ubuntu 24.04, x86-64

| Benchmark        | Stak (full [^2]) | Lua 5.4 |
| ---------------- | ---------------: | ------: |
| Empty program    |         0.534 us | 48.9 us |
| Integer addition |          22.9 us | 50.0 us |

[^1]: Minimal: Integer-only support + standard libraries based on libc
[^2]: Full: 64-bit floating-point number support + standard libraries based on the `std` library in Rust
