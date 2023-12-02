#[allow(dead_code)]
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum Type {
    #[default]
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
