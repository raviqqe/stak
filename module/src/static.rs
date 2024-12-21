//! Static modules.

use crate::{Guard, Module};
use core::ops::Deref;

/// A static module.
#[derive(Debug)]
pub struct StaticModule {
    bytecode: &'static [u8],
}

impl StaticModule {
    /// Creates a static module.
    pub const fn new(bytecode: &'static [u8]) -> Self {
        Self { bytecode }
    }
}

impl<'a> Module<'a> for StaticModule {
    type Guard = StaticGuard;

    fn bytecode(&'a self) -> Self::Guard {
        StaticGuard(self.bytecode)
    }
}

/// A read guard against a static module.
pub struct StaticGuard(&'static [u8]);

impl Deref for StaticGuard {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl Guard for StaticGuard {}
