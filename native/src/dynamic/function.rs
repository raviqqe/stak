use super::error::DynamicError;
use alloc::boxed::Box;
use core::{any::Any, mem::size_of};

/// A dynamic function.
pub struct DynamicFunction<'a> {
    arity: usize,
    #[expect(clippy::type_complexity)]
    function: Box<dyn FnMut(&[&dyn Any]) -> Result<Box<dyn Any>, DynamicError> + 'a>,
}

impl<'a> DynamicFunction<'a> {
    /// Creates a dynamic function.
    pub fn new(
        arity: usize,
        function: impl FnMut(&[&dyn Any]) -> Result<Box<dyn Any>, DynamicError> + 'a,
    ) -> Self {
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
    pub fn call(&mut self, arguments: &[&dyn Any]) -> Result<Box<dyn Any>, DynamicError> {
        (self.function)(arguments)
    }
}

/// A native function dynamically defined.
pub trait IntoDynamicFunction<'a, T, S> {
    /// Converts itself into a dynamic function.
    fn into_dynamic(self) -> DynamicFunction<'a>;
}

macro_rules! impl_function {
    ($($type:ident),*; $tuple:ty) => {
        impl<'a, T1: FnMut($($type),*) -> T2 + 'a, T2: Any, $($type: Any + Clone),*> IntoDynamicFunction<'a, $tuple, T2> for T1 {
            #[allow(non_snake_case)]
            fn into_dynamic(mut self) -> DynamicFunction<'a> {
                #[allow(unused, unused_mut)]
                DynamicFunction::new(
                    (&[$(size_of::<$type>()),*] as &[usize]).len(),
                    move |arguments: &[&dyn Any]| {
                        let mut iter = 0..;
                        $(let $type: &$type =
                            arguments[iter.next().unwrap_or_default()]
                            .downcast_ref().ok_or(DynamicError::Downcast)?;)*
                        $(let $type: $type = $type.clone();)*

                        Ok(Box::new(self($($type),*)))
                    },
                )
            }
        }
    };
}

macro_rules! impl_functions {
    ($first:ident, $($type:ident),*) => {
        impl_function!($first, $($type),*; ($first, $($type),*));
        impl_functions!($($type),*);
    };
    ($type:ident) => {
        impl_function!($type; ($type,));
        impl_function!(; ());
    }
}

impl_functions!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z);

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::{format, string::String};

    #[derive(Clone, Debug)]
    struct Foo {}

    fn foo(x: usize, y: usize) -> usize {
        x + y
    }

    fn bar(name: String, value: Option<Foo>) -> String {
        format!("{name}: {value:?}")
    }

    #[test]
    fn create_dynamic_function() {
        foo.into_dynamic();
        bar.into_dynamic();
    }
}
