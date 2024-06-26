use crate::{Error, StackedRecord};

/// Reverses stack frames in profile records.
pub fn reverse_stacks<R: StackedRecord>(
    records: impl IntoIterator<Item = Result<R, Error>>,
) -> impl IntoIterator<Item = Result<R, Error>> {
    records.into_iter().map(|record| {
        record.map(|mut record| {
            record.stack_mut().reverse_frames();
            record
        })
    })
}
