pub use primitive_set::TimePrimitiveSet;

/// A primitive of time.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Primitive {
    /// A current jiffy.
    CurrentJiffy,
}

impl Primitive {
    const CURRENT_JIFFY: usize = Self::CurrentJiffy as _;
}
