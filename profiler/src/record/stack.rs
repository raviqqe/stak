use crate::{Error, FRAME_SEPARATOR};
use core::{
    fmt::{self, Display, Formatter},
    str::FromStr,
};

/// A stack.
#[derive(Debug, Clone, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Stack {
    frames: Vec<Option<String>>,
}

impl Stack {
    /// Creates a stack.
    pub const fn new(frames: Vec<Option<String>>) -> Self {
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

    /// Collapses frames.
    pub fn collapse_frames(&mut self) {
        let mut frames = vec![];

        for frame in self.frames.drain(..) {
            if frames.last() != Some(&frame) {
                frames.push(frame);
            }
        }

        self.frames = frames;
    }

    /// Displays a stack with a fixed local names.
    pub fn display_local<'a>(&'a self, local_name: &str) -> impl Display + 'a {
        StackDisplay::new(self, local_name)
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

pub struct StackDisplay<'a> {
    stack: &'a Stack,
    local_name: &'a str,
}

impl<'a> StackDisplay<'a> {
    pub const fn new(stack: &'a Stack, local_name: &'a str) -> Self {
        Self { stack, local_name }
    }
}

impl Display for StackDisplay<'_> {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        let mut first = true;

        for frame in self.stack.frames() {
            if !first {
                write!(formatter, "{FRAME_SEPARATOR}")?;
            }

            first = false;

            write!(formatter, "{}", frame.unwrap_or(self.local_name))?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn parse() {
        let stack = Stack::new(vec![Some("foo".into()), Some("bar".into())]);

        assert_eq!(stack.to_string().parse::<Stack>().unwrap(), stack);
    }

    #[test]
    fn display_local() {
        let stack = Stack::new(vec![Some("foo".into()), None]);

        assert_eq!(stack.to_string(), "foo;");
    }

    mod collapse_frames {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn collapse_named_frames() {
            let mut stack = Stack::new(vec![
                Some("foo".into()),
                Some("foo".into()),
                Some("foo".into()),
            ]);

            stack.collapse_frames();

            assert_eq!(stack.to_string(), "foo");
        }

        #[test]
        fn collapse_anonymous_frames() {
            let mut stack = Stack::new(vec![None, None]);

            stack.collapse_frames();

            assert_eq!(stack.to_string(), "");
        }

        #[test]
        fn collapse_frames_in_middle() {
            let mut stack = Stack::new(vec![
                Some("baz".into()),
                Some("bar".into()),
                Some("bar".into()),
                Some("foo".into()),
            ]);

            stack.collapse_frames();

            assert_eq!(stack.to_string(), "baz;bar;foo");
        }
    }
}
