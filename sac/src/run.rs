use alloc::boxed::Box;
use stak_device::libc::{ReadWriteDevice, Stderr, Stdin, Stdout};
use stak_file::LibcFileSystem;
use stak_process_context::LibcProcessContext;
use stak_r7rs::{SmallError, SmallPrimitiveSet};
use stak_time::LibcClock;
use stak_vm::{Value, Vm};

/// Runs bytecode on a virtual machine.
///
/// # Safety
///
/// The `argc` and `argv` arguments should be the ones passed down as
/// arguments to the `main` function in C. Environment variables should not
/// be modified while the virtual machine runs.
pub unsafe fn run(
    bytecode: &[u8],
    heap_size: usize,
    argc: isize,
    argv: *const *const i8,
) -> Result<(), SmallError> {
    // Leak a heap to keep deallocation out of a binary.
    let heap = Box::leak(Box::<[Value]>::new_uninit_slice(heap_size));

    Vm::new(
        // SAFETY: `Value`s are scalar values.
        unsafe { heap.assume_init_mut() },
        SmallPrimitiveSet::new(
            ReadWriteDevice::new(Stdin::new(), Stdout::new(), Stderr::new()),
            LibcFileSystem::new(),
            // SAFETY: `argc` and `argv` are provided by operating systems, and
            // environment variables are never modified.
            unsafe { LibcProcessContext::new(argc, argv) },
            LibcClock::new(),
        ),
    )?
    .run(bytecode.iter().copied())
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::ptr::dangling;
    use stak_configuration::DEFAULT_HEAP_SIZE;
    use stak_macro::compile_r7rs;
    use stak_vm::Error;

    fn run_bytecode(bytecode: &[u8], heap_size: usize) -> Result<(), SmallError> {
        // SAFETY: No argument is read, and no test modifies environment
        // variables.
        unsafe { run(bytecode, heap_size, 0, dangling()) }
    }

    #[test]
    fn run_nothing() {
        assert_eq!(run_bytecode(compile_r7rs!(""), DEFAULT_HEAP_SIZE), Ok(()));
    }

    #[test]
    fn exit_with_code() {
        assert_eq!(
            run_bytecode(
                compile_r7rs!("(import (scheme process-context)) (exit 42)"),
                DEFAULT_HEAP_SIZE
            ),
            Err(SmallError::Halt(42))
        );
    }

    #[test]
    fn run_without_heap() {
        assert_eq!(
            run_bytecode(compile_r7rs!(""), 0),
            Err(SmallError::Vm(Error::OutOfMemory))
        );
    }
}
