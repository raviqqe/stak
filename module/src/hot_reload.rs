//! Hot-reloaded modules.

use crate::{Guard, Module};
use core::ops::Deref;
use hmr::Module;

/// A hot-reloaded module.
pub struct HotReloadModule {
    module: Module,
}

impl HotReloadModule {
    /// Creates a hot-reloaded module.
    pub const fn new(path: &'static str) -> Self {
        Self {
            module: Module::new(path),
        }
    }
}

impl Module<'static> for HotReloadModule {
    type Guard = HotReloadGuard;

    fn bytecode(&'static self) -> Self::Guard {
        HotReloadGuard(self.module.load())
    }
}

/// A read guard against a hot-reloaded module.
pub struct HotReloadGuard(hmr::Guard);

impl Deref for HotReloadGuard {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Guard for HotReloadGuard {}
