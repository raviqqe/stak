#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error {
    IllegalInstruction,
    MissingOperand,
}
