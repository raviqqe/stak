#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Operand {
    Global(u64),
    Local(u64),
}
