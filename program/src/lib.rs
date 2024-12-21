//! Programs in Stak Scheme.

use cfg_exif::feature;
use core::ops::Deref;
#[cfg(feature = "hot-reload")]
use hmr::Module;

/// A program.
pub struct Program {
    #[cfg(not(feature = "hot-reload"))]
    bytecode: &'static [u8],
    #[cfg(feature = "hot-reload")]
    module: Module,
}

#[cfg(not(feature = "hot-reload"))]
impl Program {
    /// Creates a program.
    pub const fn from_bytecode(bytecode: &'static [u8]) -> Self {
        Self { bytecode }
    }

    /// Returns bytecodes.
    pub const fn bytecode(&self) -> Guard {
        Guard {
            bytecode: self.bytecode,
        }
    }
}

#[cfg(feature = "hot-reload")]
impl Program {
    /// Creates a program.
    pub const fn from_path(path: &'static str) -> Self {
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
    #[cfg(not(feature = "hot-reload"))]
    bytecode: &'static [u8],
    #[cfg(feature = "hot-reload")]
    guard: hmr::Guard,
}

impl Deref for Guard {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        feature!(if ("hot-reload") {
            &*self.guard
        } else {
            self.bytecode
        })
    }
}
