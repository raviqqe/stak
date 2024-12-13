#![expect(missing_docs)]
#![no_std]

use stak_device::FixedBufferDevice;
use stak_file::VoidFileSystem;
use stak_macro::{compile_bare, compile_r7rs, include_bare, include_r7rs};
use stak_process_context::VoidProcessContext;
use stak_r7rs::SmallPrimitiveSet;
use stak_time::VoidClock;
use stak_vm::{Value, Vm};

const HEAP_SIZE: usize = 1 << 16;
const BUFFER_SIZE: usize = 1 << 10;

fn create_vm(
    heap: &mut [Value],
) -> Vm<
    SmallPrimitiveSet<
        FixedBufferDevice<BUFFER_SIZE, 0>,
        VoidFileSystem,
        VoidProcessContext,
        VoidClock,
    >,
> {
    Vm::new(
        heap,
        SmallPrimitiveSet::new(
            FixedBufferDevice::<BUFFER_SIZE, 0>::new(&[]),
            VoidFileSystem::new(),
            VoidProcessContext::new(),
            VoidClock::new(),
        ),
    )
    .unwrap()
}

mod bare {
    use super::*;

    #[test]
    fn compile_define() {
        let mut heap = [Default::default(); HEAP_SIZE];
        let mut vm = create_vm(&mut heap);

        const PROGRAM: &[u8] = compile_bare!("($$define x 42)");

        vm.initialize(PROGRAM.iter().copied()).unwrap();
        vm.run().unwrap();
    }

    #[test]
    fn include() {
        let mut heap = [Default::default(); HEAP_SIZE];
        let mut vm = create_vm(&mut heap);

        const PROGRAM: &[u8] = include_bare!("../tests/empty.scm");

        vm.initialize(PROGRAM.iter().copied()).unwrap();
        vm.run().unwrap();
    }
}

mod r7rs {
    use super::*;

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
            (import (scheme base) (scheme write))

            (define foo-bar-baz "Hello, world!")

            (display foo-bar-baz)
            "#
        );

        vm.initialize(PROGRAM.iter().copied()).unwrap();
        vm.run().unwrap();

        assert_eq!(vm.primitive_set().device().output(), b"Hello, world!");
    }

    #[test]
    fn include() {
        let mut heap = [Default::default(); HEAP_SIZE];
        let mut vm = create_vm(&mut heap);

        const PROGRAM: &[u8] = include_r7rs!("../tests/empty.scm");

        vm.initialize(PROGRAM.iter().copied()).unwrap();
        vm.run().unwrap();
    }
}
