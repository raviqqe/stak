/// A primitive of time.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Primitive {
    /// A current jiffy.
    CurrentJiffy,
    /// Jiffies per second.
    JiffiesPerSecond,
}

impl Primitive {
    pub(super) const CURRENT_JIFFY: usize = Self::CurrentJiffy as _;
    pub(super) const JIFFIES_PER_SECOND: usize = Self::JiffiesPerSecond as _;
}
