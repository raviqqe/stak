use super::{Record, StackedRecord};
use crate::{Error, ProcedureOperation, Stack, COLUMN_SEPARATOR};
use core::{
    fmt::{self, Display, Formatter},
    str::FromStr,
};

/// A procedure record.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ProcedureRecord {
    operation: ProcedureOperation,
    stack: Stack,
    time: u128,
}

impl ProcedureRecord {
    /// Creates a new record.
    pub const fn new(operation: ProcedureOperation, stack: Stack, time: u128) -> Self {
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

    /// Returns a time.
    pub const fn time(&self) -> u128 {
        self.time
    }
}

impl FromStr for ProcedureRecord {
    type Err = Error;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let mut iterator = string.split(COLUMN_SEPARATOR);

        Ok(Self::new(
            iterator
                .next()
                .ok_or(Error::MissingProcedureOperation)?
                .parse()?,
            iterator.next().ok_or(Error::MissingStack)?.parse()?,
            iterator.next().ok_or(Error::MissingTime)?.parse()?,
        ))
    }
}

impl Display for ProcedureRecord {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        write!(formatter, "{}", self.operation)?;
        write!(formatter, "{COLUMN_SEPARATOR}")?;
        write!(formatter, "{}", &self.stack)?;
        write!(formatter, "{COLUMN_SEPARATOR}")?;
        write!(formatter, "{}", &self.time)?;

        Ok(())
    }
}

impl Record for ProcedureRecord {}

impl StackedRecord for ProcedureRecord {
    fn stack(&self) -> &Stack {
        &self.stack
    }

    fn stack_mut(&mut self) -> &mut Stack {
        &mut self.stack
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn parse() {
        let record = ProcedureRecord::new(
            ProcedureOperation::Call,
            Stack::new(vec![Some("foo".into()), Some("bar".into())]),
            42,
        );

        assert_eq!(
            record.to_string().parse::<ProcedureRecord>().unwrap(),
            record
        );
    }
}
