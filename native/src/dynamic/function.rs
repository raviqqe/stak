use alloc::boxed::Box;
use core::any::Any;

/// A native function dynamically defined.
pub trait DynamicFunction {
    /// Returns a number of parameters.
    fn parameter_count(&self) -> usize;

    /// Calls a function.
    fn call(&mut self, arguments: &[&Box<dyn Any>]) -> Box<dyn Any>;
}
