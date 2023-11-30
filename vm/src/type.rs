#[allow(dead_code)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Type {
    Null,
    Boolean,
    Pair,
    Procedure,
    Symbol,
    String,
    Character,
    Vector,
    ByteVector,
}
