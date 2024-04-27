use crate::{Error, FRAME_SEPARATOR};
use core::{
    fmt::{self, Display, Formatter},
    str::FromStr,
};

/// A stack.
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
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

    /// Displays a stack with a fixed local names.
    pub fn display_local<'a>(&'a self, local_name: &'a str) -> impl Display + '_ {
        StackDisplayLocal::new(self, local_name)
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
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{}", self.display_local(""))
    }
}

pub struct StackDisplayLocal<'a> {
    stack: &'a Stack,
    local_name: &'a str,
}

impl<'a> StackDisplayLocal<'a> {
    pub fn new(stack: &'a Stack, local_name: &'a str) -> Self {
        Self { stack, local_name }
    }
}

impl Display for StackDisplayLocal<'_> {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        let mut first = true;

        for frame in self.stack.frames() {
            if !first {
                write!(formatter, "{FRAME_SEPARATOR}")?;
            }

            first = false;

            write!(formatter, "{}", frame.as_deref().unwrap_or(self.local_name))?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse() {
        let stack = Stack::new(vec![Some("foo".into()), Some("bar".into())]);

        assert_eq!(stack.to_string().parse::<Stack>().unwrap(), stack);
    }
}
