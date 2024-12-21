//! Hot-reloaded programs.

use crate::{Guard, Program};
use core::ops::Deref;
use hmr::Module;

/// A hot-reloaded program.
pub struct HotReloadProgram {
    module: Module,
}

impl HotReloadProgram {
    /// Creates a hot-reloaded program.
    pub const fn new(path: &'static str) -> Self {
        Self {
            module: Module::new(path),
        }
    }
}

impl Program<'static> for HotReloadProgram {
    type Guard = HotReloadGuard;

    fn bytecode(&'static self) -> Self::Guard {
        HotReloadGuard(self.module.load())
    }
}

/// A read guard against a hot-reloaded program.
pub struct HotReloadGuard(hmr::Guard);

impl Deref for HotReloadGuard {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Guard for HotReloadGuard {}
