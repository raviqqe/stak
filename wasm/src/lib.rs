use stak_device::ReadWriteDevice;
use stak_primitive::SmallPrimitiveSet;
use stak_vm::Vm;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn run(bytecodes: &[u8], input: &[u8], heap_size: usize) -> Result<(), JsError> {
    let mut heap = vec![Default::default(); heap_size];
    let mut output = vec![];
    let mut error = vec![];

    let mut vm = Vm::new(
        &mut heap,
        SmallPrimitiveSet::new(ReadWriteDevice::new(input, &mut output, &mut error)),
    )?;

    vm.initialize(bytecodes.iter().copied())?;

    Ok(vm.run()?)
}
