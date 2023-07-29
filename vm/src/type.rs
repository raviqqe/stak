#[allow(dead_code)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Type {
    Pair,
    Procedure,
    Symbol,
    String,
    Character,
    Vector,
    ByteVector,
    Eof,
    Port,
}
