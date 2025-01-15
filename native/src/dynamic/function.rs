use core::any::Any;

/// A native function dynamically defined.
pub trait DynamicFunction {
    /// Returns a number of parameters.
    fn parameter_count(&self) -> usize;

    /// Calls a function.
    fn call(&mut self, arguments: &[&mut dyn Any]) -> &mut dyn Any;
}
