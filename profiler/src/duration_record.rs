use crate::{Error, ProcedureOperation, COLUMN_SEPARATOR, FRAME_SEPARATOR};
use std::str::FromStr;

/// A duration record.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DurationRecord {
    stack: Stack,
    time: u128,
}

impl DurationRecord {
    /// Creates a new record.
    pub fn new(stack: Stack, time: u128) -> Self {
        Self { stack, time }
    }

    /// Returns a stack.
    pub fn stack(&self) -> &Stack {
        &self.stack
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
            iterator.next().ok_or(Error::MissingStack)?.parse()?,
            iterator.next().ok_or(Error::MissingTime)?.parse()?,
        ))
    }
}
