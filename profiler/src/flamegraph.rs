use crate::{Error, Record};

pub fn burn_flamegraph<'a>(records: impl Iterator<Item = Result<Record<'a>, Error>>) -> ! {
    todo!()
}
