---
title: Limitations on the current implementation of Stak Scheme
description: Limitations on the current implementation of Stak Scheme
---

This page describes the limitations on the current implementation of Stak Scheme. These limitations are considered transient although some of them do not have concrete resolution plans; we might be able to remove these limitations in the future versions of Stak Scheme.

# Number representation

Number representation in Stak Scheme is either [a 64-bit floating point number (IEEE 754)](https://en.wikipedia.org/wiki/IEEE_754) or a 63-bit integer.

# String and character representation

Only ASCII characters are supported in strings and characters.

# Exception handling

Certain runtime errors are not raised as exceptions. For example, you cannot catch argument count mismatch on procedure calls and out-of-memory errors in Scheme.
