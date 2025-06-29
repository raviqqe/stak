#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum Primitive {
    Acos,
    Asin,
    Atan,
    Cos,
    Exponentiation,
    Infinite,
    Logarithm,
    Nan,
    Sin,
    Sqrt,
    Tan,
}

impl Primitive {
    pub const ACOS: usize = Self::Acos as _;
    pub const ASIN: usize = Self::Asin as _;
    pub const ATAN: usize = Self::Atan as _;
    pub const COS: usize = Self::Cos as _;
    pub const EXPONENTIATION: usize = Self::Exponentiation as _;
    pub const INFINITE: usize = Self::Infinite as _;
    pub const LOGARITHM: usize = Self::Logarithm as _;
    pub const NAN: usize = Self::Nan as _;
    pub const SIN: usize = Self::Sin as _;
    pub const SQRT: usize = Self::Sqrt as _;
    pub const TAN: usize = Self::Tan as _;
}
