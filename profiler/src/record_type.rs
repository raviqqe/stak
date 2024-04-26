/// A record type.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RecordType {
    Call,
    Return,
    ReturnCall,
}
