//! A Stak Scheme interpreter in WASM.

use stak_compiler::compile_r7rs;
use stak_device::ReadWriteDevice;
use stak_file::VoidFileSystem;
use stak_process_context::VoidProcessContext;
use stak_r7rs::SmallPrimitiveSet;
use stak_time::VoidClock;
use stak_vm::Vm;
use wasm_bindgen::prelude::*;

/// Compiles a source code.
#[wasm_bindgen]
pub fn compile(source: &str) -> Result<Vec<u8>, JsError> {
    let mut target = vec![];
    compile_r7rs(source.as_bytes(), &mut target)?;
    Ok(target)
}

/// Interprets bytecodes with a standard input and returns its standard output.
#[wasm_bindgen]
pub fn interpret(bytecodes: &[u8], input: &[u8], heap_size: usize) -> Result<Vec<u8>, JsError> {
    let mut heap = vec![Default::default(); heap_size];
    let mut output = vec![];
    let mut error = vec![];

    let mut vm = Vm::new(
        &mut heap,
        SmallPrimitiveSet::new(
            ReadWriteDevice::new(input, &mut output, &mut error),
            VoidFileSystem::new(),
            VoidProcessContext::new(),
            VoidClock::new(),
        ),
    )?;

    vm.initialize(bytecodes.iter().copied())?;
    vm.run()?;

    Ok(output)
}
