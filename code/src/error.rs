#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error {
    ArgumentCount,
    ConsExpected,
    IllegalInstruction,
    IllegalPrimitive,
    NumberExpected,
    OutOfMemory,
    StackUnderflow,
}
