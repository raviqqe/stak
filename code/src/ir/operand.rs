#[derive(Clone, Copy, Debug)]
pub enum Operand {
    Global(u64),
    Local(u64),
}
