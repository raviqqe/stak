use cfg_elif::item;
use stak_device::Device;
use stak_file::VoidFileSystem;
use stak_macro::include_module;
use stak_module::Module;
use stak_process_context::VoidProcessContext;
use stak_r7rs::SmallPrimitiveSet;
use stak_time::VoidClock;
use stak_vm::Vm;
use std::io;
use wasm_bindgen::prelude::*;
use winter_maybe_async::{maybe_async, maybe_await};

item::feature!(if ("async") {
    // `maybe_async` does not work here because `wasm_bindgen`'s expansion happens
    // first.
    #[wasm_bindgen]
    extern "C" {
        async fn read_stdin() -> JsValue;
        async fn write_stdout(byte: u8);
        async fn write_stderr(byte: u8);
    }
} else {
    #[wasm_bindgen]
    extern "C" {
        fn read_stdin() -> JsValue;
        fn write_stdout(byte: u8);
        fn write_stderr(byte: u8);
    }
});

/// Runs a REPL interpreter.
#[maybe_async]
#[wasm_bindgen]
pub fn repl(heap_size: usize) -> Result<(), JsError> {
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
        include_module!("repl.scm", stak_module)
            .bytecode()
            .iter()
            .copied(),
    )?;
    maybe_await!(vm.run_async())?;

    Ok(())
}

struct JsDevice {}

impl Device for JsDevice {
    type Error = io::Error;

    #[maybe_async]
    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        let byte = maybe_await!(read_stdin());

        if byte.is_null() {
            Ok(None)
        } else {
            Ok(Some(byte.as_f64().unwrap() as _))
        }
    }

    #[maybe_async]
    fn write(&mut self, byte: u8) -> Result<(), Self::Error> {
        maybe_await!(write_stdout(byte));
        Ok(())
    }

    #[maybe_async]
    fn write_error(&mut self, byte: u8) -> Result<(), Self::Error> {
        maybe_await!(write_stderr(byte));
        Ok(())
    }
}
