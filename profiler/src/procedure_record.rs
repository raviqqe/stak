use crate::{Error, ProcedureOperation, COLUMN_SEPARATOR, FRAME_SEPARATOR};
use std::str::FromStr;

/// A procedure record.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ProcedureRecord {
    r#type: ProcedureOperation,
    stack: Vec<Option<String>>,
    time: u128,
}

impl ProcedureRecord {
    /// Creates a new record.
    pub fn new(r#type: ProcedureOperation, stack: Vec<Option<String>>, time: u128) -> Self {
        Self {
            r#type,
            stack,
            time,
        }
    }

    /// Returns a record type.
    pub const fn r#type(&self) -> ProcedureOperation {
        self.r#type
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

impl FromStr for ProcedureRecord {
    type Err = Error;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let mut iterator = string.split(COLUMN_SEPARATOR);

        Ok(ProcedureRecord::new(
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
