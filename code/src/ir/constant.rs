#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Constant {
    Symbol(u64),
    Integer(u64),
}
