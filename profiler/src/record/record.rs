use core::{fmt::Display, str::FromStr};

pub trait Record: Display + FromStr {}
