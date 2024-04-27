use crate::{Error, ProcedureOperation, COLUMN_SEPARATOR, FRAME_SEPARATOR};
use std::str::FromStr;

/// A duration record.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DurationRecord {
    stack: Vec<Option<String>>,
    time: u128,
}

impl DurationRecord {
    /// Creates a new record.
    pub fn new(stack: Vec<Option<String>>, time: u128) -> Self {
        Self { stack, time }
    }

    /// Returns a stack.
    pub fn stack(&self) -> impl Iterator<Item = Option<&str>> {
        self.stack.iter().map(Option::as_deref)
    }

    /// Returns a time.
    pub const fn time(&self) -> u128 {
        self.time
    }
}

impl FromStr for DurationRecord {
    type Err = Error;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let mut iterator = string.split(COLUMN_SEPARATOR);

        Ok(DurationRecord::new(
            iterator.next().ok_or(Error::MissingRecordType)?.parse()?,
            {
                let mut stack = iterator
                    .next()
                    .ok_or(Error::MissingStack)?
                    .split(FRAME_SEPARATOR)
                    .map(|frame| (!frame.is_empty()).then_some(frame.to_owned()))
                    .collect::<Vec<_>>();
                stack.reverse();
                stack
            },
            iterator.next().ok_or(Error::MissingTime)?.parse()?,
        ))
    }
}
