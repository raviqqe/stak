/// A type in Scheme.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum Type {
    /// A pair.
    #[default]
    Pair,
    /// A null.
    Null,
    /// A boolean.
    Boolean,
    /// A procedure.
    Procedure,
    /// A symbol.
    Symbol,
    /// A string.
    String,
    /// A character.
    Character,
    /// A vector.
    Vector,
    /// A byte vector.
    ByteVector,
}
