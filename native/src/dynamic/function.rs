use alloc::boxed::Box;
use core::any::{Any, TypeId};

/// A dynamic function.
pub struct DynamicFunction {
    arity: usize,
    function: Box<dyn Fn(&[&dyn Any]) -> Box<dyn Any>>,
}

impl DynamicFunction {
    /// Creates a dynamic function.
    pub fn new(arity: usize, function: impl Fn(&[&dyn Any]) -> Box<dyn Any> + 'static) -> Self {
        Self {
            arity,
            function: Box::new(function),
        }
    }

    /// Returns an arity.
    pub fn arity(&self) -> usize {
        self.arity
    }

    /// Calls a function.
    pub fn call(&mut self, arguments: &[&dyn Any]) -> Box<dyn Any> {
        (self.function)(arguments)
    }
}

/// A native function dynamically defined.
pub trait IntoDynamicFunction<T, S> {
    /// Converts itself into a dynamic function.
    fn into_dynamic(self) -> DynamicFunction;
}

macro_rules! impl_function {
    ($($type:ident),*) => {
        impl<F: Fn($(&$type),*) -> Z + 'static, $($type: Any),*, Z: Any> IntoDynamicFunction<($($type),*,), Z> for F {
            #[allow(non_snake_case)]
            fn into_dynamic(self) -> DynamicFunction {
                let arity = [$(TypeId::of::<$type>()),*].len();

                DynamicFunction::new(
                    arity,
                    move |arguments: &[&dyn Any]| {
                        let mut iter = 0..arity;
                        $(let $type: &$type = arguments[iter.next().unwrap()].downcast_ref().unwrap();)*
                        Box::new(self($($type),*))
                    },
                )
            }
        }
    };
}

macro_rules! impl_functions {
    ($first:ident, $($type:ident),*) => {
        impl_function!($first, $($type),*);
        impl_functions!($($type),*);
    };
    ($type:ident) => {
        impl_function!($type);
    }
}

impl_functions!(A, B, C);
