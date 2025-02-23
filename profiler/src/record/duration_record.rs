use super::{Record, StackedRecord};
use crate::{COLUMN_SEPARATOR, Error, Stack};
use core::{
    fmt::{self, Display, Formatter},
    str::FromStr,
};

/// A duration record.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DurationRecord {
    stack: Stack,
    time: u128,
}

impl DurationRecord {
    /// Creates a record.
    pub const fn new(stack: Stack, time: u128) -> Self {
        Self { stack, time }
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

        Ok(Self::new(
            iterator.next().ok_or(Error::MissingStack)?.parse()?,
            iterator.next().ok_or(Error::MissingTime)?.parse()?,
        ))
    }
}

impl Display for DurationRecord {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        write!(formatter, "{}", &self.stack)?;
        write!(formatter, "{COLUMN_SEPARATOR}")?;
        write!(formatter, "{}", &self.time)?;

        Ok(())
    }
}

impl Record for DurationRecord {}

impl StackedRecord for DurationRecord {
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

    #[test]
    fn parse() {
        let record =
            DurationRecord::new(Stack::new(vec![Some("foo".into()), Some("bar".into())]), 42);

        assert_eq!(
            record.to_string().parse::<DurationRecord>().unwrap(),
            record
        );
    }
}
