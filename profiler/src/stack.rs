use crate::{Error, FRAME_SEPARATOR};
use std::{fmt::Display, str::FromStr};

/// A stack.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Stack {
    frames: Vec<Option<String>>,
}

impl Stack {
    /// Creates a stack.
    pub fn new(frames: Vec<Option<String>>) -> Self {
        Self { frames }
    }

    /// Returns a stack.
    pub fn frames(&self) -> impl Iterator<Item = Option<&str>> {
        self.frames.iter().map(Option::as_deref)
    }

    /// Reverses frames.
    pub fn reverse_frames(&mut self) {
        self.frames.reverse();
    }
}

impl FromStr for Stack {
    type Err = Error;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        Ok(Self::new(
            string
                .split(FRAME_SEPARATOR)
                .map(|frame| (!frame.is_empty()).then_some(frame.to_owned()))
                .collect(),
        ))
    }
}

impl Display for Stack {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut first = true;

        for frame in &self.frames {
            if !first {
                write!(formatter, "{FRAME_SEPARATOR}")?;
            }

            first = false;

            write!(formatter, "{}", frame.unwrap_or_default())?;
        }

        Ok(())
    }
}
