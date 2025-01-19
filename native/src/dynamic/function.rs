use super::error::DynamicError;
use alloc::boxed::Box;
use core::{any::Any, cell::RefCell, marker::PhantomData, mem::size_of};

type AnyCell<'a> = &'a RefCell<Box<dyn Any>>;
type BoxedFunction<'a> =
    Box<dyn FnMut(&[&dyn Any], &[AnyCell]) -> Result<Box<dyn Any>, DynamicError> + 'a>;

/// A dynamic function.
pub struct DynamicFunction<'a> {
    arity: usize,
    cell_arity: usize,
    #[expect(clippy::type_complexity)]
    function: BoxedFunction<'a>,
}

impl<'a> DynamicFunction<'a> {
    /// Creates a dynamic function.
    pub fn new(arity: usize, cell_arity: usize, function: BoxedFunction<'a>) -> Self {
        Self {
            arity,
            cell_arity,
            function,
        }
    }

    /// Returns an arity of unboxed arguments.
    pub const fn arity(&self) -> usize {
        self.arity
    }

    /// Returns an arity of mutable reference arguments.
    pub const fn cell_arity(&self) -> usize {
        self.cell_arity
    }

    /// Calls a function.
    pub fn call(
        &mut self,
        arguments: &[&dyn Any],
        cell_arguments: &[AnyCell],
    ) -> Result<Box<dyn Any>, DynamicError> {
        (self.function)(arguments, cell_arguments)
    }
}

/// A native function dynamically defined.
pub trait IntoDynamicFunction<'a, T, S> {
    /// Converts itself into a dynamic function.
    fn into_dynamic(self) -> DynamicFunction<'a>;
}

struct RefMut<T> {
    _data: PhantomData<T>,
}

macro_rules! impl_function {
    ([$($type:ident),*], [$($ref:ident),*]) => {
        impl<'a, T1: FnMut($($type,)* $(&mut $ref,)*) -> T2 + 'a, T2: Any, $($type: Any + Clone,)* $($ref: Any,)*> IntoDynamicFunction<'a, ($($type,)* $(RefMut<$ref>,)*), T2> for T1 {
            #[allow(non_snake_case)]
            fn into_dynamic(mut self) -> DynamicFunction<'a> {
                #[allow(unused, unused_mut)]
                DynamicFunction::new(
                    (&[$(size_of::<$type>()),*] as &[usize]).len(),
                    (&[$(size_of::<$ref>()),*] as &[usize]).len(),
                    Box::new(move |arguments: &[&dyn Any], cell_arguments: &[AnyCell]| {
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
                                cell_arguments[ref_iter.next().unwrap_or_default()]
                                .borrow_mut()
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
    use core::cell::RefCell;

    #[derive(Clone, Debug)]
    struct Foo {}

    const fn foo(x: usize, y: usize) -> usize {
        x + y
    }

    fn bar(name: String, value: Option<Foo>) -> String {
        format!("{name}: {value:?}")
    }

    fn baz(x: usize, y: &mut usize) {
        *y = x;
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

    #[test]
    fn call_dynamic_function_with_mutable_reference() {
        let x: RefCell<Box<dyn Any>> = RefCell::new(Box::new(0usize));

        baz.into_dynamic().call(&[&42usize], &[&x]).unwrap();

        assert_eq!(*x.borrow().downcast_ref::<usize>().unwrap(), 42);
    }
}
