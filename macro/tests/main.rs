#![no_std]

use stak_device::FixedBufferDevice;
use stak_macro::compile_r7rs;
use stak_primitive::SmallPrimitiveSet;
use stak_vm::{Value, Vm};

const HEAP_SIZE: usize = 1 << 16;
const BUFFER_SIZE: usize = 1 << 10;

fn create_vm(heap: &mut [Value]) -> Vm<SmallPrimitiveSet<FixedBufferDevice<BUFFER_SIZE, 0>>> {
    Vm::new(
        heap,
        SmallPrimitiveSet::new(FixedBufferDevice::<BUFFER_SIZE, 0>::new(&[])),
    )
    .unwrap()
}

#[test]
fn compile_string() {
    let mut heap = [Default::default(); HEAP_SIZE];
    let mut vm = create_vm(&mut heap);

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
fn compile_character() {
    let mut heap = [Default::default(); HEAP_SIZE];
    let mut vm = create_vm(&mut heap);

    const PROGRAM: &[u8] = compile_r7rs!(
        r#"
        (import (scheme write))

        (display #\A)
        (display #\,)
        (display #\space)
        "#
    );

    vm.initialize(PROGRAM.iter().copied()).unwrap();
    vm.run().unwrap();

    assert_eq!(vm.primitive_set().device().output(), b"A, ");
}

#[test]
fn compile_identifier_with_hyphen() {
    let mut heap = [Default::default(); HEAP_SIZE];
    let mut vm = create_vm(&mut heap);

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
