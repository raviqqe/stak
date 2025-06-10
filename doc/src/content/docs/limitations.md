---
title: Limitations in the current implementation
description: Limitations in the current implementation of Stak Scheme
---

This page describes the limitations on the current implementation of Stak Scheme. These limitations are considered transient although some of them do not have any concrete resolution plans right now; we hopefully remove these limitations in the future versions of Stak Scheme without incurring too much complexity.

## Number representation

Number representation in Stak Scheme is either [a 64-bit floating point number (IEEE 754)](https://en.wikipedia.org/wiki/IEEE_754) or a 63-bit integer.

The other "full" implementations of [the R7RS-small standard](https://small.r7rs.org/), such as [Chibi Scheme](https://github.com/ashinn/chibi-scheme), [Gauche](https://github.com/shirok/Gauche), and [Guile](https://www.gnu.org/software/guile/), often come with [the full numeric tower](https://en.wikipedia.org/wiki/Numerical_tower).
