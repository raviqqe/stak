use crate::{Guard, HotReloadGuard, HotReloadModule, Module, StaticGuard, StaticModule};
use core::ops::Deref;

/// A universal module.
#[allow(clippy::large_enum_variant)]
pub enum UniversalModule {
    /// A hot-reloaded module.
    HotReload(HotReloadModule),
    /// A static module.
    Static(StaticModule),
}

impl UniversalModule {
    /// Creates a module from bytecodes.
    pub const fn from_bytecode(bytecode: &'static [u8]) -> Self {
        Self::Static(StaticModule::new(bytecode))
    }

    /// Creates a module from a hot-reloaded path.
    pub const fn from_hot_reload_path(path: &'static str) -> Self {
        Self::HotReload(HotReloadModule::new(path))
    }
}

impl Module<'static> for UniversalModule {
    type Guard = UniversalGuard;

    fn bytecode(&'static self) -> Self::Guard {
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
