#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum Primitive {
    Exponentiation,
    Logarithm,
}

impl Primitive {
    pub const EXPONENTIATION: usize = Self::Exponentiation as _;
    pub const LOGARITHM: usize = Self::Logarithm as _;
}
