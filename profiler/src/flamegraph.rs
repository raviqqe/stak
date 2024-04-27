use crate::{DurationRecord, Error, Stack};
use std::{collections::HashMap, io::Write};

/// Calculates a flamegraph.
pub fn calculate_flamegraph(
    records: impl IntoIterator<Item = Result<DurationRecord, Error>>,
    mut writer: impl Write,
) -> Result<(), Error> {
    let mut map = HashMap::<Stack, u128>::new();

    for record in records {
        let record = record?;

        if let Some(time) = map.get_mut(record.stack()) {
            *time += record.time();
        } else {
            map.insert(record.stack().clone(), record.time());
        }
    }

    for (key, value) in map {
        write!(writer, "{} {}", key, value)?;
    }

    Ok(())
}
