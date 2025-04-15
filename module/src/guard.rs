//! Guards against modules.

use core::ops::Deref;

/// A read guard against a module.
pub trait Guard: Deref<Target = [u8]> {}
