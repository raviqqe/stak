# Hot module reloading

This example shows how to use the feature of hot module reloading to change behavior of Rust programs dynamically embedding Scheme scripts.

## Usage

```sh
cargo run &
curl -f -X POST --data '(1 2 3 4 5)' http://localhost:3000/sum
```
