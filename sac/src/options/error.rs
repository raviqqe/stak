use core::{
    error,
    fmt::{self, Display, Formatter},
};

/// An option error.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum OptionError {
    /// An invalid heap size.
    HeapSize,
}

impl error::Error for OptionError {}

impl Display for OptionError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::HeapSize => write!(formatter, "invalid heap size"),
        }
    }
}
