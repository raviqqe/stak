//! Programs in Stak Scheme.

use cfg_exif::feature;
use core::ops::Deref;
#[cfg(feature = "hmr")]
use hmr::Module;

/// A program.
pub struct Program {
    #[cfg(not(feature = "hmr"))]
    bytecode: &'static [u8],
    #[cfg(feature = "hmr")]
    module: Module,
}

#[cfg(not(feature = "hmr"))]
impl Program {
    /// Creates a program.
    pub const fn new(bytecode: &'static [u8]) -> Self {
        Self { bytecode }
    }

    /// Returns bytecodes.
    pub const fn bytecode(&self) -> Guard {
        Guard {
            bytecode: self.bytecode,
        }
    }
}

#[cfg(feature = "hmr")]
impl Program {
    /// Creates a program.
    pub const fn new(path: &'static str) -> Self {
        Self {
            module: Module::new(path),
        }
    }

    /// Returns bytecodes.
    pub fn bytecode(&'static self) -> Guard {
        Guard {
            guard: self.module.load(),
        }
    }
}

/// A read guard against a program.
pub struct Guard {
    #[cfg(not(feature = "hmr"))]
    bytecode: &'static [u8],
    #[cfg(feature = "hmr")]
    guard: hmr::Guard,
}

impl Deref for Guard {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        feature!(if ("hmr") { &*self.guard } else { self.bytecode })
    }
}
