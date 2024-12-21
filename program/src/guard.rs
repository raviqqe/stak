//! Guards against programs.

use core::ops::Deref;

/// A read guard against a program.
pub trait Guard: Deref<Target = [u8]> {}