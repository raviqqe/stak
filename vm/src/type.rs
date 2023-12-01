#[allow(dead_code)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Type {
    Pair,
    Null,
    Boolean,
    Procedure,
    Symbol,
    String,
    Character,
    Vector,
    ByteVector,
}
