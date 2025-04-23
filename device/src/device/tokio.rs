use crate::Device;
use stak_async::AsyncContext;
use tokio::io::{AsyncReadExt, AsyncWriteExt, Error, Stderr, Stdin, Stdout, stderr, stdin, stdout};

/// A standard I/O device of a current process.
pub struct TokioDevice<'a> {
    context: &'a AsyncContext<'a>,
    stdin: Stdin,
    stdout: Stdout,
    stderr: Stderr,
}

impl<'a> TokioDevice<'a> {
    /// Creates a device.
    pub fn new(context: &'a AsyncContext<'a>) -> Self {
        Self {
            context,
            stdin: stdin(),
            stdout: stdout(),
            stderr: stderr(),
        }
    }
}

impl<'a> Device for TokioDevice<'a> {
    type Error = Error;

    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        self.context.r#yield(self.stdin.read_u8())?
    }

    fn write(&mut self, byte: u8) -> Result<(), Self::Error> {
        let _future = self.stdout.write_u8(byte);

        todo!("store future into async context")
    }

    fn write_error(&mut self, byte: u8) -> Result<(), Self::Error> {
        let _future = self.stderr.write_u8(byte);

        todo!("store future into async context")
    }
}
