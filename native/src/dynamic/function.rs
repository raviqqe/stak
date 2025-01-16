use alloc::boxed::Box;
use core::any::{Any, TypeId};

/// A dynamic function.
pub struct DynamicFunction {
    arity: usize,
    #[expect(clippy::type_complexity)]
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
    pub const fn arity(&self) -> usize {
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
        impl<T1: Fn($(&$type),*) -> T2 + 'static, $($type: Any),*, T2: Any> IntoDynamicFunction<($($type),*,), Z> for T1 {
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

impl_functions!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z);
