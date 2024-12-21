//! Static programs.

use crate::{Guard, Program};
use core::ops::Deref;

/// A static program.
#[derive(Debug)]
pub struct StaticProgram {
    bytecode: &'static [u8],
}

impl StaticProgram {
    /// Creates a static program.
    pub const fn new(bytecode: &'static [u8]) -> Self {
        Self { bytecode }
    }
}

impl<'a> Program<'a> for StaticProgram {
    type Guard = StaticGuard;

    fn bytecode(&'a self) -> Self::Guard {
        StaticGuard(self.bytecode)
    }
}

/// A read guard against a static program.
pub struct StaticGuard(&'static [u8]);

impl Deref for StaticGuard {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl Guard for StaticGuard {}
