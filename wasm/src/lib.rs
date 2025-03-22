//! A Stak Scheme interpreter in WASM.

use stak_compiler::compile_r7rs;
use stak_device::ReadWriteDevice;
use stak_file::{MemoryFileSystem, VoidFileSystem};
use stak_macro::include_module;
use stak_module::Module;
use stak_process_context::{MemoryProcessContext, VoidProcessContext};
use stak_r7rs::SmallPrimitiveSet;
use stak_time::VoidClock;
use stak_vm::Vm;
use wasm_bindgen::prelude::*;

/// Compiles source codes in Scheme.
#[wasm_bindgen]
pub fn compile(source: &str) -> Result<Vec<u8>, JsError> {
    let mut target = vec![];
    compile_r7rs(source.as_bytes(), &mut target)?;
    Ok(target)
}

/// Interprets bytecodes with standard input and returns its standard output.
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

/// Runs a Scheme script with standard input and returns its standard output.
#[wasm_bindgen]
pub fn run(source: &str, input: &[u8], heap_size: usize) -> Result<Vec<u8>, JsError> {
    const MAIN_FILE: &str = "main.scm";

    let mut heap = vec![Default::default(); heap_size];
    let mut output = vec![];
    let mut error = vec![];
    let files = [(MAIN_FILE.as_bytes(), source.as_bytes())];
    let mut file_entries = [Default::default(); 1];

    let mut vm = Vm::new(
        &mut heap,
        SmallPrimitiveSet::new(
            ReadWriteDevice::new(input, &mut output, &mut error),
            MemoryFileSystem::new(&files, &mut file_entries),
            MemoryProcessContext::new(&["scheme", MAIN_FILE], &[]),
            VoidClock::new(),
        ),
    )?;

    vm.initialize(
        include_module!("run.scm", stak_module)
            .bytecode()
            .iter()
            .copied(),
    )?;
    vm.run().map_err(|other| {
        if error.is_empty() {
            JsError::from(other)
        } else {
            JsError::new(error)
        }
    })?;

    Ok(output)
}
