use stak_compiler::compile_r7rs;
use stak_device::ReadWriteDevice;
use stak_primitive::SmallPrimitiveSet;
use stak_vm::Vm;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn compile(source: &str) -> Result<Vec<u8>, JsError> {
    let mut target = vec![];
    compile_r7rs(source.as_bytes(), &mut target)?;
    Ok(target)
}

#[wasm_bindgen]
pub fn interpret(bytecodes: &[u8], input: &[u8], heap_size: usize) -> Result<Vec<u8>, JsError> {
    let mut heap = vec![Default::default(); heap_size];
    let mut output = vec![];
    let mut error = vec![];

    let mut vm = Vm::new(
        &mut heap,
        SmallPrimitiveSet::new(ReadWriteDevice::new(input, &mut output, &mut error)),
    )?;

    vm.initialize(bytecodes.iter().copied())?;
    vm.run()?;

    Ok(output)
}
