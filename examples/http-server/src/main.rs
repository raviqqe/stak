//! A `stak-build` example.

use axum::{http::StatusCode, routing::post, Router};
use stak_device::ReadWriteDevice;
use stak_file::VoidFileSystem;
use stak_macro::include_bytecode;
use stak_process_context::VoidProcessContext;
use stak_r7rs::SmallPrimitiveSet;
use stak_time::VoidClock;
use stak_vm::Vm;
use std::error::Error;

const HEAP_SIZE: usize = 1 << 16;
const BUFFER_SIZE: usize = 1 << 10;
const ROOT_BYTECODES: &[u8] = include_bytecode!("handler.scm");

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let app = Router::new().route("/sum", post(sum));

    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await?;
    axum::serve(listener, app).await?;

    Ok(())
}

async fn sum() -> axum::response::Result<(StatusCode, Vec<u8>)> {
    let mut output = vec![0u8; BUFFER_SIZE];
    let mut error = vec![0u8; BUFFER_SIZE];

    run(ROOT_BYTECODES, &[], &mut output, &mut error).map_err(|error| error.to_string())?;

    if !error.is_empty() {
        return Ok((StatusCode::BAD_REQUEST, error));
    }

    Ok((StatusCode::OK, output))
}

fn run(
    bytecodes: &[u8],
    input: &[u8],
    output: &mut [u8],
    error: &mut [u8],
) -> Result<(), stak_r7rs::SmallError> {
    let mut heap = vec![Default::default(); HEAP_SIZE];
    let mut vm = Vm::new(
        &mut heap,
        SmallPrimitiveSet::new(
            ReadWriteDevice::new(input, output, error),
            VoidFileSystem::new(),
            VoidProcessContext::new(),
            VoidClock::new(),
        ),
    )?;

    vm.initialize(bytecodes.iter().copied())?;
    vm.run()
}
