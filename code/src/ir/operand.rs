#[derive(Debug)]
pub enum Operand {
    Global(usize),
    Local(usize),
}
