# Hot reloading

This example shows how to use the feature of hot reloading to change behavior of Rust programs dynamically embedding Scheme scripts.

## Usage

```sh
# Start an HTTP server.
cargo run
# In another terminal, send an HTTP request to the server.
curl -f -X POST --data '(1 2 3 4 5)' http://localhost:3000/sum # -> 15
```

Then, you can modify the Scheme script at `src/handler.scm` and run `cargo build`. It should change the behavior of the HTTP server while it keeps running.
