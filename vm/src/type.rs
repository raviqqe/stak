#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Type {
    Pair,
    Procedure,
    Singleton,
    String,
    Symbol,
}
