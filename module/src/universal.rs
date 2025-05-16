use crate::{Guard, HotReloadGuard, HotReloadModule, Module, StaticGuard, StaticModule};
use core::ops::Deref;

/// A universal module.
pub enum UniversalModule {
    /// A hot-reloaded module.
    HotReload(&'static HotReloadModule),
    /// A static module.
    Static(StaticModule),
}

impl<'a> Module<'a> for UniversalModule {
    type Guard = UniversalGuard;

    fn bytecode(&'a self) -> Self::Guard {
        match self {
            Self::HotReload(module) => UniversalGuard::HotReload(module.bytecode()),
            Self::Static(module) => UniversalGuard::Static(module.bytecode()),
        }
    }
}

/// A read guard against a universal module.
pub enum UniversalGuard {
    /// A guard for a hot-reloaded module.
    HotReload(HotReloadGuard),
    /// A guard for a static module.
    Static(StaticGuard),
}

impl Deref for UniversalGuard {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        match self {
            Self::HotReload(guard) => guard,
            Self::Static(guard) => guard,
        }
    }
}

impl Guard for UniversalGuard {}
