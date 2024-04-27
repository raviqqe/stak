use crate::RecordType;

/// A record.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Record {
    r#type: RecordType,
    stack: Vec<Option<String>>,
    time: u128,
}

impl Record {
    /// Creates a new record.
    pub fn new(r#type: RecordType, stack: Vec<Option<String>>, time: u128) -> Self {
        Self {
            r#type,
            stack,
            time,
        }
    }

    /// Returns a record type.
    pub const fn r#type(&self) -> RecordType {
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
