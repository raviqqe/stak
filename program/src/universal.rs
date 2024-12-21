use crate::{Guard, HotReloadGuard, HotReloadProgram, Program, StaticGuard, StaticProgram};
use core::ops::Deref;

/// A universal program.
#[allow(clippy::large_enum_variant)]
pub enum UniversalProgram {
    /// A hot-reloaded program.
    HotReload(HotReloadProgram),
    /// A static program.
    Static(StaticProgram),
}

impl UniversalProgram {
    /// Creates a program from bytecodes.
    pub const fn from_bytecode(bytecode: &'static [u8]) -> Self {
        Self::Static(StaticProgram::new(bytecode))
    }

    /// Creates a program from a hot-reloaded path.
    pub const fn from_hot_reload_path(path: &'static str) -> Self {
        Self::HotReload(HotReloadProgram::new(path))
    }
}

impl Program<'static> for UniversalProgram {
    type Guard = UniversalGuard;

    fn bytecode(&'static self) -> Self::Guard {
        match self {
            Self::HotReload(program) => UniversalGuard::HotReload(program.bytecode()),
            Self::Static(program) => UniversalGuard::Static(program.bytecode()),
        }
    }
}

/// A read guard against a universal program.
pub enum UniversalGuard {
    /// A guard for a hot-reloaded program.
    HotReload(HotReloadGuard),
    /// A guard for a static program.
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
