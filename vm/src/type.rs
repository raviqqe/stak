#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Type {
    Pair,
    Procedure,
    String,
    Symbol,
    False,
    True,
    Null,
}
