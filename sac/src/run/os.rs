use alloc::vec;
use stak_device::StdioDevice;
use stak_file::OsFileSystem;
use stak_process_context::OsProcessContext;
use stak_r7rs::{SmallError, SmallPrimitiveSet};
use stak_time::OsClock;
use stak_vm::Vm;

/// Runs bytecode on a virtual machine.
pub fn run(bytecode: &[u8], heap_size: usize) -> Result<(), SmallError> {
    Vm::new(
        vec![Default::default(); heap_size],
        SmallPrimitiveSet::new(
            StdioDevice::new(),
            OsFileSystem::new(),
            OsProcessContext::new(),
            OsClock::new(),
        ),
    )?
    .run(bytecode.iter().copied())
}

#[cfg(test)]
mod tests {
    use super::*;
    use stak_configuration::DEFAULT_HEAP_SIZE;
    use stak_macro::compile_r7rs;
    use stak_vm::Error;

    #[test]
    fn run_nothing() {
        assert_eq!(run(compile_r7rs!(""), DEFAULT_HEAP_SIZE), Ok(()));
    }

    #[test]
    fn exit_with_code() {
        assert_eq!(
            run(
                compile_r7rs!("(import (scheme process-context)) (exit 42)"),
                DEFAULT_HEAP_SIZE
            ),
            Err(SmallError::Halt(42))
        );
    }

    #[test]
    fn run_without_heap() {
        assert_eq!(
            run(compile_r7rs!(""), 0),
            Err(SmallError::Vm(Error::OutOfMemory))
        );
    }
}
