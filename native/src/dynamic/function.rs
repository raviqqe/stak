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
    ([$($type:ident),*], [$($ref:ident),*]) => {
        impl<'a, T1: FnMut($($type,)* $(&mut $ref,)*) -> T2 + 'a, T2: Any, $($type: Any + Clone,)* $($ref: Any,)*> IntoDynamicFunction<'a, ($($type,)* $(&mut $ref,)*), T2> for T1 {
            #[allow(non_snake_case)]
            fn into_dynamic(mut self) -> DynamicFunction<'a> {
                #[allow(unused, unused_mut)]
                DynamicFunction::new(
                    (&[$(size_of::<$type>()),*] as &[usize]).len(),
                    (&[$(size_of::<$ref>()),*] as &[usize]).len(),
                    Box::new(move |arguments: &[&dyn Any], arguments_mut: &[&mut dyn Any]| {
                        let mut iter = 0..;
                        let mut ref_iter = 0..;

                        Ok(Box::new(self(
                            $(
                                arguments[iter.next().unwrap_or_default()]
                                .downcast_ref::<$type>()
                                .ok_or(DynamicError::Downcast)?
                                .clone(),
                            )*
                            $(
                                arguments_mut[ref_iter.next().unwrap_or_default()]
                                .downcast_mut::<$ref>()
                                .ok_or(DynamicError::Downcast)?,
                            )*
                        )))
                    }),
                )
            }
        }
    };
}

macro_rules! impl_ref_functions {
    ([$($type:ident),*], [$first_ref:ident, $($ref:ident),*]) => {
        impl_function!([$($type),*], [$first_ref, $($ref),*]);
        impl_ref_functions!([$($type),*], [$($ref),*]);
    };
    ([$($type:ident),*], [$ref:ident]) => {
        impl_function!([$($type),*], [$ref]);
        impl_function!([$($type),*], []);
    }
}

macro_rules! impl_functions {
    ([$first_type:ident, $($type:ident),*], [$($ref:ident),*]) => {
        impl_ref_functions!([$first_type, $($type),*], [$($ref),*]);
        impl_functions!([$($type),*], [$($ref),*]);
    };
    ([$type:ident], [$($ref:ident),*]) => {
        impl_ref_functions!([$type], [$($ref),*]);
        impl_ref_functions!([], [$($ref),*]);
    }
}

impl_functions!(
    [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z],
    [い, ろ, は, に, お, へ, と, ち, り, ぬ, る, を]
);

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
