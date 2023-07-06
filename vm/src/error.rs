#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error {
    ArgumentCount,
    ConsExpected,
    EndOfInput,
    IllegalInstruction,
    IllegalPrimitive,
    MissingOperand,
    NumberExpected,
    OutOfMemory,
    StackUnderflow,
}
