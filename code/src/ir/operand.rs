#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Operand {
    Symbol(u64),
    Integer(u64),
}
