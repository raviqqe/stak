use super::Record;
use crate::Stack;

pub trait StackedRecord: Record {
    /// Returns a stack.
    fn stack(&self) -> &Stack;

    /// Returns a mutable stack.
    fn stack_mut(&mut self) -> &mut Stack;
}
