#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error {
    ArgumentCount,
    IllegalInstruction,
    IllegalPrimitive,
}
