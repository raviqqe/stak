use alloc::boxed::Box;
use core::cell::RefCell;
use core::ops::{Deref, DerefMut};

pub struct Mut<'a, T: ?Sized>(&'a RefCell<Box<T>>);

impl<'a, T: ?Sized> Mut<'a, T> {
    pub fn new(value: &'a RefCell<Box<T>>) -> Self {
        Self(value)
    }
}

impl<'a, T: ?Sized> Deref for Mut<'a, T> {
    type Target = &'a RefCell<Box<T>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a, T: ?Sized> DerefMut for Mut<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
