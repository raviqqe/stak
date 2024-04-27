use crate::{DurationRecord, Error};
use std::{collections::HashMap, io::Write};

/// Calculates a flamegraph.
pub fn calculate_flamegraph(
    _records: impl IntoIterator<Item = Result<DurationRecord, Error>>,
    mut _writer: impl Write,
) -> Result<(), Error> {
    let mut _map = HashMap::<String, u128>::new();

    todo!()
}
