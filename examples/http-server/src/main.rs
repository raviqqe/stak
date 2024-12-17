//! A `stak-build` example.

use axum::{http::StatusCode, response, routing::post, serve, Router};
use core::{error::Error, ffi::CStr};
use stak::{
    device::ReadWriteDevice,
    file::VoidFileSystem,
    include_bytecode,
    process_context::VoidProcessContext,
    r7rs::{SmallError, SmallPrimitiveSet},
    time::VoidClock,
    vm::Vm,
};

const HEAP_SIZE: usize = 1 << 16;
const BUFFER_SIZE: usize = 1 << 10;
const ROOT_BYTECODES: &[u8] = include_bytecode!("handler.scm");

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    serve(
        tokio::net::TcpListener::bind("0.0.0.0:3000").await?,
        Router::new().route("/sum", post(sum)),
    )
    .await?;

    Ok(())
}

async fn sum(input: String) -> response::Result<(StatusCode, String)> {
    let mut output = [0u8; BUFFER_SIZE];
    let mut error = [0u8; BUFFER_SIZE];

    run(ROOT_BYTECODES, input.as_bytes(), &mut output, &mut error)
        .map_err(|error| error.to_string())?;

    let error = decode_buffer(&error)?;

    Ok(if error.is_empty() {
        (StatusCode::OK, decode_buffer(&output)?)
    } else {
        (StatusCode::BAD_REQUEST, error)
    })
}

fn decode_buffer(buffer: &[u8]) -> response::Result<String> {
    Ok(CStr::from_bytes_until_nul(buffer)
        .map_err(|error| error.to_string())?
        .to_string_lossy()
        .into())
}

fn run(
    bytecodes: &[u8],
    input: &[u8],
    output: &mut [u8],
    error: &mut [u8],
) -> Result<(), SmallError> {
    let mut heap = [Default::default(); HEAP_SIZE];
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
