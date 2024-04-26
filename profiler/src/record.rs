use crate::RecordType;

/// A record.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Record<'a> {
    r#type: RecordType,
    stack: Vec<&'a str>,
    time: u128,
}

impl<'a> Record<'a> {
    /// Creates a new record.
    pub fn new(r#type: RecordType, stack: Vec<&'a str>, time: u128) -> Self {
        Self {
            r#type,
            stack,
            time,
        }
    }

    /// Returns a record type.
    pub fn r#type(&self) -> RecordType {
        self.r#type
    }
    /// Returns a stack.
    pub fn stack(&self) -> impl DoubleEndedIterator<Item = &str> {
        self.stack.iter().map(AsRef::as_ref)
    }
    /// Returns a time.
    pub fn time(&self) -> u128 {
        self.time
    }
}
