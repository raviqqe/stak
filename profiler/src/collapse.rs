use crate::{DurationRecord, Error};
use std::io::Write;

/// Collapses stacks.
pub fn collapse_stacks(
    records: impl IntoIterator<Item = Result<DurationRecord, Error>>,
    mut writer: impl Write,
) -> Result<(), Error> {
    for record in records {
        let mut record = record?;

        record.stack_mut().collapse;

        write!(writer, "{}", record)?;
    }

    Ok(())
}
