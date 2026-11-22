/// Command line options.
#[derive(Clone, Copy, Debug)]
pub struct Options {
    version: bool,
    heap_size: Option<usize>,
}

impl Options {
    /// Parses command line arguments.
    ///
    /// Options are the arguments between a command name and the first `--`
    /// argument. Unknown and invalid ones are ignored.
    pub fn parse(arguments: &[impl AsRef<str>]) -> Self {
        let arguments = arguments
            .get(1..)
            .unwrap_or_default()
            .split(|argument| argument.as_ref() == "--")
            .next()
            .unwrap_or_default();

        Self {
            version: arguments
                .iter()
                .any(|argument| matches!(argument.as_ref(), "-V" | "--version")),
            heap_size: arguments
                .iter()
                .enumerate()
                .rev()
                .find_map(|(index, argument)| match argument.as_ref() {
                    "-s" | "--heap-size" => arguments.get(index + 1)?.as_ref().parse().ok(),
                    argument => argument.strip_prefix("--heap-size=")?.parse().ok(),
                }),
        }
    }

    /// Returns `true` if a version is requested.
    pub const fn version(&self) -> bool {
        self.version
    }

    /// Returns a heap size.
    pub const fn heap_size(&self) -> Option<usize> {
        self.heap_size
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_nothing() {
        let options = Options::parse(&[""; 0]);

        assert!(!options.version());
        assert_eq!(options.heap_size(), None);
    }

    #[test]
    fn parse_command_name() {
        let options = Options::parse(&["stak"]);

        assert!(!options.version());
        assert_eq!(options.heap_size(), None);
    }

    #[test]
    fn parse_arguments() {
        let options = Options::parse(&["stak", "foo.scm", "bar"]);

        assert!(!options.version());
        assert_eq!(options.heap_size(), None);
    }

    #[test]
    fn parse_options() {
        let options = Options::parse(&["stak", "--version", "--heap-size", "42"]);

        assert!(options.version());
        assert_eq!(options.heap_size(), Some(42));
    }

    mod version {
        use super::*;

        #[test]
        fn parse_short_option() {
            assert!(Options::parse(&["stak", "-V"]).version());
        }

        #[test]
        fn parse_long_option() {
            assert!(Options::parse(&["stak", "--version"]).version());
        }

        #[test]
        fn parse_option_after_argument() {
            assert!(Options::parse(&["stak", "foo.scm", "--version"]).version());
        }

        #[test]
        fn parse_option_after_unknown_option() {
            assert!(Options::parse(&["stak", "--foo", "--version"]).version());
        }

        #[test]
        fn parse_option_before_delimiter() {
            assert!(Options::parse(&["stak", "--version", "--"]).version());
        }

        #[test]
        fn ignore_unknown_option() {
            assert!(!Options::parse(&["stak", "-v"]).version());
            assert!(!Options::parse(&["stak", "--versions"]).version());
        }

        #[test]
        fn ignore_option_after_delimiter() {
            assert!(!Options::parse(&["stak", "--", "--version"]).version());
        }

        #[test]
        fn ignore_command_name() {
            assert!(!Options::parse(&["--version"]).version());
        }
    }

    mod heap_size {
        use super::*;

        #[test]
        fn parse_short_option() {
            assert_eq!(Options::parse(&["stak", "-s", "42"]).heap_size(), Some(42));
        }

        #[test]
        fn parse_long_option() {
            assert_eq!(
                Options::parse(&["stak", "--heap-size", "42"]).heap_size(),
                Some(42)
            );
        }

        #[test]
        fn parse_long_option_with_equal_sign() {
            assert_eq!(
                Options::parse(&["stak", "--heap-size=42"]).heap_size(),
                Some(42)
            );
        }

        #[test]
        fn parse_option_after_argument() {
            assert_eq!(
                Options::parse(&["stak", "foo.scm", "-s", "42"]).heap_size(),
                Some(42)
            );
        }

        #[test]
        fn parse_option_after_unknown_option() {
            assert_eq!(
                Options::parse(&["stak", "--foo", "-s", "42"]).heap_size(),
                Some(42)
            );
        }

        #[test]
        fn parse_option_before_delimiter() {
            assert_eq!(
                Options::parse(&["stak", "-s", "42", "--"]).heap_size(),
                Some(42)
            );
        }

        #[test]
        fn parse_last_option() {
            assert_eq!(
                Options::parse(&["stak", "-s", "1", "--heap-size=2", "--heap-size", "3"])
                    .heap_size(),
                Some(3)
            );
        }

        #[test]
        fn parse_option_before_invalid_options() {
            assert_eq!(
                Options::parse(&["stak", "-s", "42", "--heap-size=foo", "-s"]).heap_size(),
                Some(42)
            );
        }

        #[test]
        fn ignore_missing_value() {
            assert_eq!(Options::parse(&["stak", "-s"]).heap_size(), None);
            assert_eq!(Options::parse(&["stak", "--heap-size"]).heap_size(), None);
            assert_eq!(Options::parse(&["stak", "--heap-size="]).heap_size(), None);
        }

        #[test]
        fn ignore_invalid_value() {
            assert_eq!(Options::parse(&["stak", "-s", "foo"]).heap_size(), None);
            assert_eq!(Options::parse(&["stak", "-s", "-1"]).heap_size(), None);
            assert_eq!(Options::parse(&["stak", "-s", "4.2"]).heap_size(), None);
            assert_eq!(
                Options::parse(&["stak", "--heap-size=foo"]).heap_size(),
                None
            );
        }

        #[test]
        fn ignore_short_option_with_attached_value() {
            assert_eq!(Options::parse(&["stak", "-s42"]).heap_size(), None);
            assert_eq!(Options::parse(&["stak", "-s=42"]).heap_size(), None);
        }

        #[test]
        fn ignore_option_after_delimiter() {
            assert_eq!(
                Options::parse(&["stak", "--", "-s", "42"]).heap_size(),
                None
            );
        }

        #[test]
        fn ignore_value_after_delimiter() {
            assert_eq!(
                Options::parse(&["stak", "-s", "--", "42"]).heap_size(),
                None
            );
        }

        #[test]
        fn ignore_command_name() {
            assert_eq!(Options::parse(&["-s", "42"]).heap_size(), None);
        }
    }
}
