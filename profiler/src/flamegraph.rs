use crate::Error;
use std::{collections::HashMap, io::Write};

/// Calculates a flamegraph.
pub fn calculate_flamegraph(
    records: impl IntoIterator<Item = Result<DurationRecord, Error>>,
    mut writer: impl Write,
) -> Result<(), Error> {
    let mut map = HashMap::new();
    todo!()
}
