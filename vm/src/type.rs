#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Type {
    #[allow(dead_code)]
    Pair,
    Procedure,
    Symbol,
    String,
    Character,
}
