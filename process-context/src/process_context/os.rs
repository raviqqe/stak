use crate::ProcessContext;
use alloc::{string::String, vec::Vec};
use core::ops::Range;
use std::{
    env::{args, vars},
    sync::LazyLock,
};

/// A process context provided by an operating system.
pub struct OsProcessContext {
    arguments: LazyLock<Vec<String>>,
    environment_variables: LazyLock<Vec<(String, String)>>,
    argument_skip: Range<usize>,
}

impl OsProcessContext {
    /// Creates a process context.
    pub fn new() -> Self {
        Self {
            arguments: LazyLock::new(|| args().collect()),
            environment_variables: LazyLock::new(|| vars().collect()),
            argument_skip: 0..0,
        }
    }

    /// Sets a range of arguments to skip.
    pub fn with_argument_skip(self, skip: Range<usize>) -> Self {
        Self {
            argument_skip: skip,
            ..self
        }
    }
}

impl ProcessContext for OsProcessContext {
    fn command_line_rev(&self) -> impl IntoIterator<Item = &str> {
        let end = self.argument_skip.end.min(self.arguments.len());

        self.arguments[..self.argument_skip.start.min(end)]
            .iter()
            .chain(&self.arguments[end..])
            .map(AsRef::as_ref)
            .rev()
    }

    fn environment_variables(&self) -> impl IntoIterator<Item = (&str, &str)> {
        (*self.environment_variables)
            .iter()
            .map(|(key, value)| (key.as_ref(), value.as_ref()))
    }
}

impl Default for OsProcessContext {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn command_line_rev() {
        assert_eq!(
            OsProcessContext::new()
                .command_line_rev()
                .into_iter()
                .collect::<Vec<_>>(),
            args().rev().collect::<Vec<_>>()
        );
    }

    mod with_argument_skip {
        use super::*;

        fn command_line_rev(skip: Range<usize>) -> Vec<String> {
            OsProcessContext {
                arguments: LazyLock::new(|| ["foo", "bar", "baz", "qux"].map(Into::into).into()),
                ..OsProcessContext::new()
            }
            .with_argument_skip(skip)
            .command_line_rev()
            .into_iter()
            .map(Into::into)
            .collect()
        }

        #[test]
        fn skip_first_arguments() {
            assert_eq!(command_line_rev(0..2), ["qux", "baz"]);
        }

        #[test]
        fn skip_middle_arguments() {
            assert_eq!(command_line_rev(1..3), ["qux", "foo"]);
        }

        #[test]
        fn skip_last_arguments() {
            assert_eq!(command_line_rev(2..4), ["bar", "foo"]);
        }

        #[test]
        fn skip_empty_range() {
            assert_eq!(command_line_rev(2..2), ["qux", "baz", "bar", "foo"]);
        }

        #[test]
        #[expect(clippy::reversed_empty_ranges)]
        fn skip_inverted_range() {
            assert_eq!(command_line_rev(3..1), ["qux", "baz", "bar", "foo"]);
        }

        #[test]
        fn skip_range_across_end() {
            assert_eq!(command_line_rev(2..6), ["bar", "foo"]);
        }

        #[test]
        fn skip_range_after_end() {
            assert_eq!(command_line_rev(5..7), ["qux", "baz", "bar", "foo"]);
        }
    }
}
