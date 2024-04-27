use crate::{Error, ProcedureOperation, Stack, COLUMN_SEPARATOR};
use std::str::FromStr;

/// A procedure record.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ProcedureRecord {
    operation: ProcedureOperation,
    stack: Stack,
    time: u128,
}

impl ProcedureRecord {
    /// Creates a new record.
    pub fn new(operation: ProcedureOperation, stack: Stack, time: u128) -> Self {
        Self {
            operation,
            stack,
            time,
        }
    }

    /// Returns a procedure operation.
    pub const fn operation(&self) -> ProcedureOperation {
        self.operation
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

impl FromStr for ProcedureRecord {
    type Err = Error;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let mut iterator = string.split(COLUMN_SEPARATOR);

        Ok(ProcedureRecord::new(
            iterator
                .next()
                .ok_or(Error::MissingProcedureOperation)?
                .parse()?,
            {
                let mut stack = iterator
                    .next()
                    .ok_or(Error::MissingStack)?
                    .parse::<Stack>()?;
                stack.reverse_frames();
                stack
            },
            iterator.next().ok_or(Error::MissingTime)?.parse()?,
        ))
    }
}
