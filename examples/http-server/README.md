# HTTP server

This example shows how to change behaviour of an HTTP server dynamically embedding a Scheme script.

## Usage

```sh
cargo run &
curl -f -X POST --data '(1 2 3 4 5)' http://localhost:3000/sum
```
