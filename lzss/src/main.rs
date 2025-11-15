use stak_lzss::Lzss;
use std::io::{Write, stdin, stdout};

fn main() {
    stdout().write(stdin().compress())
}
