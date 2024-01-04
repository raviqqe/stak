mod decoder;

use crate::{decode::decoder::Decoder, Error, Program};

/// Decodes a program.
pub fn decode(codes: &[u8]) -> Result<Program, Error> {
    Decoder::new(codes).decode()
}
