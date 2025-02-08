# No-`std` nad no-`alloc` environment

This example shows how to embed and run Stak Scheme in a crate without `std` and `alloc` crates in Rust.

## Usage

```sh
cargo test
```

Then, you can modify the Scheme script at `src/handler.scm` and run `cargo build`. It should change the behavior of the HTTP server while it keeps running.
