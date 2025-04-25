//! Scheme source code minifier.

use stak_configuration::DEFAULT_HEAP_SIZE;
use stak_device::ReadWriteDevice;
use stak_file::VoidFileSystem;
use stak_macro::include_r7rs;
use stak_process_context::VoidProcessContext;
use stak_r7rs::{SmallError, SmallPrimitiveSet};
use stak_time::VoidClock;
use stak_vm::Vm;
use std::io::{Read, Write, empty};

/// Minifies given source codes.
pub fn minify(reader: impl Read, writer: impl Write) -> Result<(), SmallError> {
    const BYTECODE: &[u8] = include_r7rs!("minify.scm");

    let mut heap = vec![Default::default(); DEFAULT_HEAP_SIZE];
    let mut vm = Vm::new(
        &mut heap,
        SmallPrimitiveSet::new(
            ReadWriteDevice::new(reader, writer, empty()),
            VoidFileSystem::new(),
            VoidProcessContext::new(),
            VoidClock::new(),
        ),
    )?;

    vm.initialize(BYTECODE.iter().copied())?;
    vm.run_sync()?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn minify_expression() {
        let source = "(foo  bar)";
        let mut target = vec![];

        minify(source.as_bytes(), &mut target).unwrap();

        assert_eq!(target, b"(foo bar)\n");
    }

    #[test]
    fn minify_expressions() {
        let source = "(foo  bar)\n\n(  baz   blah  )";
        let mut target = vec![];

        minify(source.as_bytes(), &mut target).unwrap();

        assert_eq!(target, b"(foo bar)\n(baz blah)\n");
    }
}
