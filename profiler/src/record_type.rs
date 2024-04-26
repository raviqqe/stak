/// A record type.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RecordType {
    /// A call.
    Call,
    /// A return.
    Return,
    /// A return (tail) call.
    ReturnCall,
}
