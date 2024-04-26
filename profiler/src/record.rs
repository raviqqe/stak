use crate::RecordType;

/// A record.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Record {
    r#type: RecordType,
    stack: Vec<String>,
    time: u128,
}
