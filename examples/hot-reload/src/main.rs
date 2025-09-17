//! A `stak-build` example.

use axum::{Router, http::StatusCode, response, routing::post, serve};
use core::error::Error;
use stak::{
    device::ReadWriteDevice,
    file::VoidFileSystem,
    include_module,
    module::Module,
    process_context::VoidProcessContext,
    r7rs::{SmallError, SmallPrimitiveSet},
    time::VoidClock,
    vm::Vm,
};

const HEAP_SIZE: usize = 1 << 16;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    serve(
        tokio::net::TcpListener::bind("0.0.0.0:3000").await?,
        Router::new().route("/calculate", post(calculate)),
    )
    .await?;

    Ok(())
}

async fn calculate(input: String) -> response::Result<(StatusCode, String)> {
    let mut output = vec![];
    let mut error = vec![];

    run(
        &include_module!("handler.scm").bytecode(),
        input.as_bytes(),
        &mut output,
        &mut error,
    )
    .map_err(|error| error.to_string())?;

    let error = decode_buffer(error)?;

    Ok(if error.is_empty() {
        (StatusCode::OK, decode_buffer(output)?)
    } else {
        (StatusCode::BAD_REQUEST, error)
    })
}

fn run(
    bytecode: &[u8],
    input: &[u8],
    output: &mut Vec<u8>,
    error: &mut Vec<u8>,
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

    vm.initialize(bytecode.iter().copied())?;
    vm.run()
}

#[expect(clippy::result_large_err)]
fn decode_buffer(buffer: Vec<u8>) -> response::Result<String> {
    Ok(String::from_utf8(buffer).map_err(|error| error.to_string())?)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn calculate_sum() {
        assert_eq!(
            calculate("(2 3)".into()).await.unwrap(),
            (StatusCode::OK, "5".into())
        );
    }
}
