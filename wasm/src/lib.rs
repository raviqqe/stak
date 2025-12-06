//! A Stak Scheme interpreter in WASM.

mod repl;

use core::str;
pub use repl::repl;
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

/// Interprets bytecode with standard input and returns its standard output.
#[wasm_bindgen]
pub fn interpret(bytecode: &[u8], input: &[u8], heap_size: usize) -> Result<Vec<u8>, JsError> {
    let mut output = vec![];
    let mut error = vec![];

    Vm::new(
        vec![Default::default(); heap_size],
        SmallPrimitiveSet::new(
            ReadWriteDevice::new(input, &mut output, &mut error),
            VoidFileSystem::new(),
            VoidProcessContext::new(),
            VoidClock::new(),
        ),
    )?
    .run(bytecode.iter().copied())?;

    Ok(output)
}

/// Runs a Scheme script with standard input and returns its standard output.
#[wasm_bindgen]
pub fn run(source: &str, input: &[u8], heap_size: usize) -> Result<Vec<u8>, JsError> {
    const MAIN_FILE: &str = "main.scm";

    let mut output = vec![];
    let mut error = vec![];
    let files = [(MAIN_FILE.as_bytes(), source.as_bytes())];
    let mut file_entries = [Default::default(); 1];

    Vm::new(
        vec![Default::default(); heap_size],
        SmallPrimitiveSet::new(
            ReadWriteDevice::new(input, &mut output, &mut error),
            MemoryFileSystem::new(&files, &mut file_entries),
            MemoryProcessContext::new(&["scheme", MAIN_FILE], &[]),
            VoidClock::new(),
        ),
    )?
    .run(
        include_module!("run.scm", stak_module)
            .bytecode()
            .iter()
            .copied(),
    )
    .map_err(|vm_error| match str::from_utf8(&error) {
        Ok(error) if !error.is_empty() => JsError::new(error),
        Ok(_) => JsError::from(vm_error),
        Err(error) => JsError::from(error),
    })?;

    Ok(output)
}
