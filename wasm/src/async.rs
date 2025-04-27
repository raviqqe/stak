use core::error::Error;
use core::fmt::{self, Display, Formatter};
use stak_device::Device;
use stak_file::VoidFileSystem;
use stak_macro::include_module;
use stak_module::Module;
use stak_process_context::VoidProcessContext;
use stak_r7rs::SmallPrimitiveSet;
use stak_time::VoidClock;
use stak_vm::Vm;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    async fn read_stdin() -> JsValue;
    async fn write_stdout(byte: u8);
    async fn write_stderr(byte: u8);
}

/// Runs a REPL interpreter.
#[wasm_bindgen]
pub async fn repl(heap_size: usize) -> Result<(), JsError> {
    let mut heap = vec![Default::default(); heap_size];

    let mut vm = Vm::new(
        &mut heap,
        SmallPrimitiveSet::new(
            JsDevice {},
            VoidFileSystem::new(),
            VoidProcessContext::new(),
            VoidClock::new(),
        ),
    )?;

    vm.initialize(
        include_module!("run.scm", stak_module)
            .bytecode()
            .iter()
            .copied(),
    )?;
    vm.run().await?;

    Ok(())
}

struct JsDevice {}

impl Device for JsDevice {
    type Error = DeviceError;

    async fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        let byte = read_stdin().await;

        if byte.is_null() {
            Ok(None)
        } else {
            Ok(Some(byte.as_f64().unwrap() as u8))
        }
    }

    async fn write(&mut self, byte: u8) -> Result<(), Self::Error> {
        write_stdout(byte).await;
        Ok(())
    }

    async fn write_error(&mut self, byte: u8) -> Result<(), Self::Error> {
        write_stderr(byte).await;
        Ok(())
    }
}

#[derive(Debug)]
pub enum DeviceError {}

impl Error for DeviceError {}

impl Display for DeviceError {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        write!(formatter, "{:?}", self)
    }
}
