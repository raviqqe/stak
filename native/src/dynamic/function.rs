use alloc::boxed::Box;
use core::any::Any;

/// A native function dynamically defined.
pub trait DynamicFunction {
    /// Returns a number of parameters.
    fn parameter_count(&self) -> usize;

    /// Calls a function.
    fn call(&mut self, arguments: &[&dyn Any]) -> Box<dyn Any>;
}

macro_rules! impl_fn {
    ($($type:ident),*) => {
        impl<F: Fn($(&$type),*) -> R, $($type: Any),*, R: Any> DynamicFunction for F {
            fn parameter_count(&self) -> usize {
                $(let $type = 0;)*
                [$($type),*].len()
            }

            fn call(&mut self, arguments: &[&dyn Any]) -> Box<dyn Any> {
                let mut iter = 0..self.parameter_count();
                $(let $type: &$type = arguments[iter.next().unwrap()].downcast_ref().unwrap();)*

                Box::new(self($($type),*))
            }
        }
    };
}

//impl_fn!(T1);
impl_fn!(T1, T2);
//impl_fn!(T1, T2, T3);
