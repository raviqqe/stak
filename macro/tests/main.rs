#![no_std]

use stak_device::FixedBufferDevice;
use stak_macro::compile_r7rs;
use stak_primitive::SmallPrimitiveSet;
use stak_vm::Vm;

#[test]
fn compile_string() {
    const HEAP_SIZE: usize = 1 << 16;
    const BUFFER_SIZE: usize = 1 << 10;

    let mut heap = [Default::default(); HEAP_SIZE];
    let device = FixedBufferDevice::<BUFFER_SIZE, 0>::new(&[]);
    let mut vm = Vm::new(&mut heap, SmallPrimitiveSet::new(device)).unwrap();

    const PROGRAM: &[u8] = compile_r7rs!(
        r#"
        (import (scheme write))

        (display "Hello, world!")
        "#
    );

    vm.initialize(PROGRAM.iter().copied()).unwrap();
    vm.run().unwrap();

    assert_eq!(vm.primitive_set().device().output(), b"Hello, world!");
}

#[test]
fn compile_identifier_with_hyphen() {
    const HEAP_SIZE: usize = 1 << 16;
    const BUFFER_SIZE: usize = 1 << 10;

    let mut heap = [Default::default(); HEAP_SIZE];
    let device = FixedBufferDevice::<BUFFER_SIZE, 0>::new(&[]);
    let mut vm = Vm::new(&mut heap, SmallPrimitiveSet::new(device)).unwrap();

    const PROGRAM: &[u8] = compile_r7rs!(
        r#"
        (import (scheme write))

        (define foo-bar-baz "Hello, world!")

        (display foo-bar-baz)
        "#
    );

    vm.initialize(PROGRAM.iter().copied()).unwrap();
    vm.run().unwrap();

    assert_eq!(vm.primitive_set().device().output(), b"Hello, world!");
}
