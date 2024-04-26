use crate::{Error, Record};
use std::io::Write;

pub fn burn_flamegraph<'a>(
    records: impl Iterator<Item = Result<Record<'a>, Error>>,
    writer: impl Write,
) -> ! {
    todo!()
}
