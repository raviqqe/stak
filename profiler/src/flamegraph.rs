use crate::{Error, Record};
use std::io::{self, Write};

pub fn burn_flamegraph<'a>(
    records: impl Iterator<Item = Result<Record<'a>, Error>>,
    mut writer: impl Write,
) -> Result<(), io::Error> {
    for record in records {
        writeln!(writer)?;
    }

    Ok(())
}
