/// An error that happens in a composite primitive set.
pub trait CompositeError {
    fn is_illegal_primitive(&self) -> bool;
}
