use super::error::DynamicError;
use alloc::boxed::Box;
use core::{any::Any, mem::size_of};

/// A dynamic function.
pub struct DynamicFunction<'a> {
    arity: usize,
    arity_mut: usize,
    #[expect(clippy::type_complexity)]
    function:
        Box<dyn FnMut(&[&dyn Any], &[&mut dyn Any]) -> Result<Box<dyn Any>, DynamicError> + 'a>,
}

impl<'a> DynamicFunction<'a> {
    /// Creates a dynamic function.
    pub fn new(
        arity: usize,
        arity_mut: usize,
        function: Box<
            dyn FnMut(&[&dyn Any], &[&mut dyn Any]) -> Result<Box<dyn Any>, DynamicError> + 'a,
        >,
    ) -> Self {
        Self {
            arity,
            arity_mut,
            function,
        }
    }

    /// Returns an arity of unboxed arguments.
    pub const fn arity(&self) -> usize {
        self.arity
    }

    /// Returns an arity of mutable reference arguments.
    pub const fn arity_mut(&self) -> usize {
        self.arity_mut
    }

    /// Calls a function.
    pub fn call(
        &mut self,
        arguments: &[&dyn Any],
        arguments_mut: &[&mut dyn Any],
    ) -> Result<Box<dyn Any>, DynamicError> {
        (self.function)(arguments, arguments_mut)
    }
}

/// A native function dynamically defined.
pub trait IntoDynamicFunction<'a, T, S> {
    /// Converts itself into a dynamic function.
    fn into_dynamic(self) -> DynamicFunction<'a>;
}

macro_rules! impl_function {
    ([$($type:ident),*], $tuple:ty) => {
        impl<'a, T1: FnMut($($type),*) -> T2 + 'a, T2: Any, $($type: Any + Clone),*> IntoDynamicFunction<'a, $tuple, T2> for T1 {
            #[allow(non_snake_case)]
            fn into_dynamic(mut self) -> DynamicFunction<'a> {
                #[allow(unused, unused_mut)]
                DynamicFunction::new(
                    (&[$(size_of::<$type>()),*] as &[usize]).len(),
                    0,
                    Box::new(move |arguments: &[&dyn Any], arguments_mut: &[&mut dyn Any]| {
                        let mut iter = 0..;

                        Ok(Box::new(self($(
                            arguments[iter.next().unwrap_or_default()]
                            .downcast_ref::<$type>()
                            .ok_or(DynamicError::Downcast)?
                            .clone()
                        ),*)))
                    }),
                )
            }
        }
    };
}

macro_rules! impl_functions {
    ($first:ident, $($type:ident),*) => {
        impl_function!([$first, $($type),*], ($first, $($type),*));
        impl_functions!($($type),*);
    };
    ($type:ident) => {
        impl_function!([$type], ($type,));
        impl_function!([], ());
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

    #[test]
    fn call_dynamic_function() {
        assert_eq!(
            *foo.into_dynamic()
                .call(&[&1usize, &2usize], &[])
                .unwrap()
                .downcast::<usize>()
                .unwrap(),
            3
        );
    }
}
