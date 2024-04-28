use crate::Stack;

pub trait StackedRecord {
    const fn stack(&self) -> &Stack;
    fn stack_mut(&mut self) -> &mut Stack;
}
