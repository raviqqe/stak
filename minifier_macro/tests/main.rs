#![no_std]

use stak_device::FixedBufferDevice;
use stak_macro::{compile_bare, compile_r7rs, include_bare, include_r7rs};
use stak_primitive::SmallPrimitiveSet;
use stak_vm::{Value, Vm};

#[test]
fn minify_expressions() {
    const PROGRAM: &str = include!("( foo  bar )\n\n(baz)");

    assert_eq!(PROGRAM, "(foo bar)\n(baz)");
}
