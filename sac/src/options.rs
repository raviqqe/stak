#[cfg(feature = "libc")]
use core::{ffi::CStr, slice};

/// Command line options.
#[derive(Debug)]
pub struct Options {
    version: bool,
    heap_size: Option<usize>,
}

impl Options {
    /// Parses command line arguments.
    pub fn parse(arguments: impl IntoIterator<Item = impl AsRef<str>>) -> Self {
        let mut version = false;
        let mut heap_size = None;
        let mut heap_size_option = false;

        let mut arguments = arguments.into_iter();
        arguments.next();

        for argument in arguments {
            let argument = argument.as_ref();

            if argument == "--" {
                break;
            } else if heap_size_option && let Ok(size) = argument.parse() {
                heap_size = Some(size);
            }

            version |= argument == "--version";
            heap_size_option = matches!(argument, "-s" | "--heap-size");
        }

        Self { version, heap_size }
    }

    /// Parses command line arguments in C.
    ///
    /// # Safety
    ///
    /// The `argc` and `argv` arguments should be the ones passed down as
    /// arguments to the `main` function in C.
    #[cfg(feature = "libc")]
    pub unsafe fn parse_c(argc: isize, argv: *const *const i8) -> Self {
        Self::parse(
            // SAFETY: Operating systems guarantee the length of `argv` to be
            // `argc`.
            unsafe { slice::from_raw_parts(argv, argc as _) }
                .iter()
                .map(|&argument| {
                    // SAFETY: Operating systems guarantee elements in `argv` to
                    // be C strings.
                    unsafe { CStr::from_ptr(argument as _) }
                        .to_str()
                        .unwrap_or_default()
                }),
        )
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
        let options = Options::parse([""; 0]);

        assert!(!options.version());
        assert_eq!(options.heap_size(), None);
    }

    #[test]
    fn parse_command_name() {
        let options = Options::parse(["stak"]);

        assert!(!options.version());
        assert_eq!(options.heap_size(), None);
    }

    #[test]
    fn parse_arguments() {
        let options = Options::parse(["stak", "foo.scm", "bar"]);

        assert!(!options.version());
        assert_eq!(options.heap_size(), None);
    }

    #[test]
    fn parse_options() {
        let options = Options::parse(["stak", "--version", "--heap-size", "42"]);

        assert!(options.version());
        assert_eq!(options.heap_size(), Some(42));
    }

    mod version {
        use super::*;

        #[test]
        fn parse_option() {
            assert!(Options::parse(["stak", "--version"]).version());
        }

        #[test]
        fn parse_option_after_argument() {
            assert!(Options::parse(["stak", "foo.scm", "--version"]).version());
        }

        #[test]
        fn parse_option_after_unknown_option() {
            assert!(Options::parse(["stak", "--foo", "--version"]).version());
        }

        #[test]
        fn parse_option_after_heap_size_option() {
            assert!(Options::parse(["stak", "-s", "--version"]).version());
        }

        #[test]
        fn parse_option_before_delimiter() {
            assert!(Options::parse(["stak", "--version", "--"]).version());
        }

        #[test]
        fn ignore_unknown_option() {
            assert!(!Options::parse(["stak", "-V"]).version());
            assert!(!Options::parse(["stak", "--versions"]).version());
        }

        #[test]
        fn ignore_option_after_delimiter() {
            assert!(!Options::parse(["stak", "--", "--version"]).version());
        }

        #[test]
        fn ignore_command_name() {
            assert!(!Options::parse(["--version"]).version());
        }
    }

    mod heap_size {
        use super::*;

        #[test]
        fn parse_short_option() {
            assert_eq!(Options::parse(["stak", "-s", "42"]).heap_size(), Some(42));
        }

        #[test]
        fn parse_long_option() {
            assert_eq!(
                Options::parse(["stak", "--heap-size", "42"]).heap_size(),
                Some(42)
            );
        }

        #[test]
        fn parse_option_after_argument() {
            assert_eq!(
                Options::parse(["stak", "foo.scm", "-s", "42"]).heap_size(),
                Some(42)
            );
        }

        #[test]
        fn parse_option_after_unknown_option() {
            assert_eq!(
                Options::parse(["stak", "--foo", "-s", "42"]).heap_size(),
                Some(42)
            );
        }

        #[test]
        fn parse_option_after_missing_value() {
            assert_eq!(
                Options::parse(["stak", "-s", "-s", "42"]).heap_size(),
                Some(42)
            );
        }

        #[test]
        fn parse_option_before_argument() {
            assert_eq!(
                Options::parse(["stak", "-s", "42", "43"]).heap_size(),
                Some(42)
            );
        }

        #[test]
        fn parse_option_before_delimiter() {
            assert_eq!(
                Options::parse(["stak", "-s", "42", "--"]).heap_size(),
                Some(42)
            );
        }

        #[test]
        fn parse_last_option() {
            assert_eq!(
                Options::parse(["stak", "-s", "1", "--heap-size", "2"]).heap_size(),
                Some(2)
            );
        }

        #[test]
        fn parse_option_before_invalid_options() {
            assert_eq!(
                Options::parse(["stak", "-s", "42", "--heap-size", "foo", "-s"]).heap_size(),
                Some(42)
            );
        }

        #[test]
        fn ignore_unknown_option() {
            assert_eq!(Options::parse(["stak", "-S", "42"]).heap_size(), None);
            assert_eq!(
                Options::parse(["stak", "--heap-sizes", "42"]).heap_size(),
                None
            );
        }

        #[test]
        fn ignore_missing_value() {
            assert_eq!(Options::parse(["stak", "-s"]).heap_size(), None);
            assert_eq!(Options::parse(["stak", "--heap-size"]).heap_size(), None);
        }

        #[test]
        fn ignore_invalid_value() {
            assert_eq!(Options::parse(["stak", "-s", "foo"]).heap_size(), None);
            assert_eq!(Options::parse(["stak", "-s", "-1"]).heap_size(), None);
            assert_eq!(Options::parse(["stak", "-s", "4.2"]).heap_size(), None);
            assert_eq!(
                Options::parse(["stak", "-s", "18446744073709551616"]).heap_size(),
                None
            );
        }

        #[test]
        fn ignore_option_with_attached_value() {
            assert_eq!(Options::parse(["stak", "-s42"]).heap_size(), None);
            assert_eq!(Options::parse(["stak", "-s=42"]).heap_size(), None);
            assert_eq!(Options::parse(["stak", "--heap-size=42"]).heap_size(), None);
        }

        #[test]
        fn ignore_option_after_delimiter() {
            assert_eq!(Options::parse(["stak", "--", "-s", "42"]).heap_size(), None);
        }

        #[test]
        fn ignore_value_after_delimiter() {
            assert_eq!(Options::parse(["stak", "-s", "--", "42"]).heap_size(), None);
        }

        #[test]
        fn ignore_command_name() {
            assert_eq!(Options::parse(["-s", "42"]).heap_size(), None);
        }
    }

    #[cfg(feature = "libc")]
    mod parse_c {
        use super::*;
        use alloc::vec::Vec;

        fn parse(arguments: &[&CStr]) -> Options {
            let arguments = arguments
                .iter()
                .map(|argument| argument.as_ptr().cast())
                .collect::<Vec<_>>();

            // SAFETY: The arguments are an array of C strings.
            unsafe { Options::parse_c(arguments.len() as _, arguments.as_ptr()) }
        }

        #[test]
        fn parse_nothing() {
            let options = parse(&[]);

            assert!(!options.version());
            assert_eq!(options.heap_size(), None);
        }

        #[test]
        fn parse_version() {
            assert!(parse(&[c"stak", c"--version"]).version());
        }

        #[test]
        fn parse_heap_size() {
            assert_eq!(parse(&[c"stak", c"-s", c"42"]).heap_size(), Some(42));
        }

        #[test]
        fn parse_options_after_invalid_utf8() {
            let options = parse(&[c"stak", c"\xff", c"--version", c"-s", c"42"]);

            assert!(options.version());
            assert_eq!(options.heap_size(), Some(42));
        }

        #[test]
        fn parse_option_after_invalid_utf8_command_name() {
            assert!(parse(&[c"\xff", c"--version"]).version());
        }
    }
}
