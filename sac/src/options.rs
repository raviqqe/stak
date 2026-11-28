mod error;

#[cfg(feature = "libc")]
use core::{ffi::CStr, slice};
use error::OptionError;

/// Command line options.
#[derive(Debug)]
pub struct Options {
    version: bool,
    heap_size: Option<usize>,
    argument_index: usize,
}

impl Options {
    /// Parses command line arguments.
    pub fn parse(
        arguments: impl IntoIterator<Item = impl AsRef<str>>,
    ) -> Result<Self, OptionError> {
        let mut version = false;
        let mut heap_size = None;
        let mut heap_size_option = false;
        let mut argument_index = 1;

        for argument in arguments.into_iter().skip(1) {
            let argument = argument.as_ref();

            if heap_size_option {
                heap_size = Some(argument.parse().map_err(|_| OptionError::HeapSize)?);
                heap_size_option = false;
            } else if argument == "--" {
                argument_index += 1;
                break;
            } else if argument == "--version" {
                version = true;
            } else if matches!(argument, "-s" | "--heap-size") {
                heap_size_option = true;
            } else {
                break;
            }

            argument_index += 1;
        }

        if heap_size_option {
            return Err(OptionError::HeapSize);
        }

        Ok(Self {
            version,
            heap_size,
            argument_index,
        })
    }

    /// Parses command line arguments in C.
    ///
    /// # Safety
    ///
    /// The `argc` and `argv` arguments should be the ones passed down as
    /// arguments to the `main` function in C.
    #[cfg(feature = "libc")]
    pub unsafe fn parse_c(argc: isize, argv: *const *const i8) -> Result<Self, OptionError> {
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

    /// Returns an index of the first argument after options.
    pub const fn argument_index(&self) -> usize {
        self.argument_index
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_nothing() {
        let options = Options::parse([""; 0]).unwrap();

        assert!(!options.version());
        assert_eq!(options.heap_size(), None);
        assert_eq!(options.argument_index(), 1);
    }

    #[test]
    fn parse_command_name() {
        let options = Options::parse(["stak"]).unwrap();

        assert!(!options.version());
        assert_eq!(options.heap_size(), None);
        assert_eq!(options.argument_index(), 1);
    }

    #[test]
    fn parse_arguments() {
        let options = Options::parse(["stak", "foo.scm", "bar"]).unwrap();

        assert!(!options.version());
        assert_eq!(options.heap_size(), None);
        assert_eq!(options.argument_index(), 1);
    }

    #[test]
    fn parse_options() {
        let options =
            Options::parse(["stak", "--version", "--heap-size", "42", "foo.scm"]).unwrap();

        assert!(options.version());
        assert_eq!(options.heap_size(), Some(42));
        assert_eq!(options.argument_index(), 4);
    }

    mod version {
        use super::*;

        #[test]
        fn parse_option() {
            assert!(Options::parse(["stak", "--version"]).unwrap().version());
        }

        #[test]
        fn parse_option_before_argument() {
            assert!(
                Options::parse(["stak", "--version", "foo.scm"])
                    .unwrap()
                    .version()
            );
        }

        #[test]
        fn parse_option_after_heap_size_option() {
            assert!(
                Options::parse(["stak", "-s", "42", "--version"])
                    .unwrap()
                    .version()
            );
        }

        #[test]
        fn parse_option_before_delimiter() {
            assert!(
                Options::parse(["stak", "--version", "--"])
                    .unwrap()
                    .version()
            );
        }

        #[test]
        fn parse_repeated_option() {
            assert!(
                Options::parse(["stak", "--version", "--version"])
                    .unwrap()
                    .version()
            );
        }

        #[test]
        fn ignore_unknown_option() {
            assert!(!Options::parse(["stak", "-V"]).unwrap().version());
            assert!(!Options::parse(["stak", "--versions"]).unwrap().version());
        }

        #[test]
        fn ignore_option_after_argument() {
            assert!(
                !Options::parse(["stak", "foo.scm", "--version"])
                    .unwrap()
                    .version()
            );
        }

        #[test]
        fn ignore_option_after_unknown_option() {
            assert!(
                !Options::parse(["stak", "--foo", "--version"])
                    .unwrap()
                    .version()
            );
        }

        #[test]
        fn ignore_option_after_delimiter() {
            assert!(
                !Options::parse(["stak", "--", "--version"])
                    .unwrap()
                    .version()
            );
        }

        #[test]
        fn ignore_command_name() {
            assert!(!Options::parse(["--version"]).unwrap().version());
        }
    }

    mod heap_size {
        use super::*;

        #[test]
        fn parse_short_option() {
            assert_eq!(
                Options::parse(["stak", "-s", "42"]).unwrap().heap_size(),
                Some(42)
            );
        }

        #[test]
        fn parse_long_option() {
            assert_eq!(
                Options::parse(["stak", "--heap-size", "42"])
                    .unwrap()
                    .heap_size(),
                Some(42)
            );
        }

        #[test]
        fn parse_option_before_argument() {
            assert_eq!(
                Options::parse(["stak", "-s", "42", "foo.scm"])
                    .unwrap()
                    .heap_size(),
                Some(42)
            );
        }

        #[test]
        fn parse_option_before_delimiter() {
            assert_eq!(
                Options::parse(["stak", "-s", "42", "--"])
                    .unwrap()
                    .heap_size(),
                Some(42)
            );
        }

        #[test]
        fn parse_last_option() {
            assert_eq!(
                Options::parse(["stak", "-s", "1", "--heap-size", "2"])
                    .unwrap()
                    .heap_size(),
                Some(2)
            );
        }

        #[test]
        fn ignore_unknown_option() {
            assert_eq!(
                Options::parse(["stak", "-S", "42"]).unwrap().heap_size(),
                None
            );
            assert_eq!(
                Options::parse(["stak", "--heap-sizes", "42"])
                    .unwrap()
                    .heap_size(),
                None
            );
        }

        #[test]
        fn ignore_option_with_attached_value() {
            assert_eq!(Options::parse(["stak", "-s42"]).unwrap().heap_size(), None);
            assert_eq!(Options::parse(["stak", "-s=42"]).unwrap().heap_size(), None);
            assert_eq!(
                Options::parse(["stak", "--heap-size=42"])
                    .unwrap()
                    .heap_size(),
                None
            );
        }

        #[test]
        fn ignore_option_after_argument() {
            assert_eq!(
                Options::parse(["stak", "foo.scm", "-s", "42"])
                    .unwrap()
                    .heap_size(),
                None
            );
        }

        #[test]
        fn ignore_missing_value_after_argument() {
            assert_eq!(
                Options::parse(["stak", "foo.scm", "-s"])
                    .unwrap()
                    .heap_size(),
                None
            );
        }

        #[test]
        fn ignore_option_after_delimiter() {
            assert_eq!(
                Options::parse(["stak", "--", "-s", "42"])
                    .unwrap()
                    .heap_size(),
                None
            );
        }

        #[test]
        fn ignore_command_name() {
            assert_eq!(Options::parse(["-s", "42"]).unwrap().heap_size(), None);
        }

        #[test]
        fn reject_missing_value() {
            assert_eq!(
                Options::parse(["stak", "-s"]).unwrap_err(),
                OptionError::HeapSize
            );
            assert_eq!(
                Options::parse(["stak", "--heap-size"]).unwrap_err(),
                OptionError::HeapSize
            );
        }

        #[test]
        fn reject_invalid_value() {
            assert_eq!(
                Options::parse(["stak", "-s", "foo"]).unwrap_err(),
                OptionError::HeapSize
            );
            assert_eq!(
                Options::parse(["stak", "-s", "-1"]).unwrap_err(),
                OptionError::HeapSize
            );
            assert_eq!(
                Options::parse(["stak", "-s", "4.2"]).unwrap_err(),
                OptionError::HeapSize
            );
            assert_eq!(
                Options::parse(["stak", "-s", "18446744073709551616"]).unwrap_err(),
                OptionError::HeapSize
            );
            assert_eq!(
                Options::parse(["stak", "-s", ""]).unwrap_err(),
                OptionError::HeapSize
            );
        }

        #[test]
        fn reject_option_as_value() {
            assert_eq!(
                Options::parse(["stak", "-s", "--version"]).unwrap_err(),
                OptionError::HeapSize
            );
            assert_eq!(
                Options::parse(["stak", "-s", "-s", "42"]).unwrap_err(),
                OptionError::HeapSize
            );
        }

        #[test]
        fn reject_delimiter_as_value() {
            assert_eq!(
                Options::parse(["stak", "-s", "--", "42"]).unwrap_err(),
                OptionError::HeapSize
            );
        }

        #[test]
        fn reject_missing_value_after_version_option() {
            assert_eq!(
                Options::parse(["stak", "--version", "-s"]).unwrap_err(),
                OptionError::HeapSize
            );
        }

        #[test]
        fn reject_invalid_value_after_option() {
            assert_eq!(
                Options::parse(["stak", "-s", "42", "--heap-size", "foo"]).unwrap_err(),
                OptionError::HeapSize
            );
        }
    }

    mod argument_index {
        use super::*;

        #[test]
        fn parse_no_argument_after_options() {
            assert_eq!(
                Options::parse(["stak", "--version", "-s", "42"])
                    .unwrap()
                    .argument_index(),
                4
            );
        }

        #[test]
        fn parse_argument_after_delimiter() {
            assert_eq!(
                Options::parse(["stak", "--", "foo.scm"])
                    .unwrap()
                    .argument_index(),
                2
            );
        }

        #[test]
        fn parse_argument_after_options_and_delimiter() {
            assert_eq!(
                Options::parse(["stak", "-s", "42", "--", "foo.scm"])
                    .unwrap()
                    .argument_index(),
                4
            );
        }

        #[test]
        fn parse_no_argument_after_delimiter() {
            assert_eq!(Options::parse(["stak", "--"]).unwrap().argument_index(), 2);
        }

        #[test]
        fn parse_unknown_option() {
            assert_eq!(
                Options::parse(["stak", "--foo", "bar"])
                    .unwrap()
                    .argument_index(),
                1
            );
        }

        #[test]
        fn parse_empty_argument() {
            assert_eq!(Options::parse(["stak", ""]).unwrap().argument_index(), 1);
        }

        #[test]
        fn parse_number() {
            assert_eq!(Options::parse(["stak", "42"]).unwrap().argument_index(), 1);
        }

        #[test]
        fn parse_number_after_heap_size_option() {
            assert_eq!(
                Options::parse(["stak", "-s", "42", "43"])
                    .unwrap()
                    .argument_index(),
                3
            );
        }

        #[test]
        fn ignore_delimiter_after_argument() {
            assert_eq!(
                Options::parse(["stak", "foo.scm", "--"])
                    .unwrap()
                    .argument_index(),
                1
            );
        }

        #[test]
        fn ignore_delimiter_after_delimiter() {
            assert_eq!(
                Options::parse(["stak", "--", "--"])
                    .unwrap()
                    .argument_index(),
                2
            );
        }

        #[test]
        fn ignore_command_name() {
            assert_eq!(
                Options::parse(["--", "foo.scm"]).unwrap().argument_index(),
                1
            );
        }
    }

    #[cfg(feature = "libc")]
    mod parse_c {
        use super::*;

        fn parse<const N: usize>(arguments: [&CStr; N]) -> Result<Options, OptionError> {
            let arguments = arguments.map(|argument| argument.as_ptr().cast());

            // SAFETY: The arguments are an array of C strings.
            unsafe { Options::parse_c(N as _, arguments.as_ptr()) }
        }

        #[test]
        fn parse_nothing() {
            let options = parse([]).unwrap();

            assert!(!options.version());
            assert_eq!(options.heap_size(), None);
            assert_eq!(options.argument_index(), 1);
        }

        #[test]
        fn parse_version() {
            assert!(parse([c"stak", c"--version"]).unwrap().version());
        }

        #[test]
        fn parse_heap_size() {
            assert_eq!(
                parse([c"stak", c"-s", c"42"]).unwrap().heap_size(),
                Some(42)
            );
        }

        #[test]
        fn parse_argument_index() {
            assert_eq!(
                parse([c"stak", c"--", c"foo.scm"])
                    .unwrap()
                    .argument_index(),
                2
            );
        }

        #[test]
        fn parse_options_before_invalid_utf8_argument() {
            let options = parse([c"stak", c"--version", c"-s", c"42", c"\xff"]).unwrap();

            assert!(options.version());
            assert_eq!(options.heap_size(), Some(42));
            assert_eq!(options.argument_index(), 4);
        }

        #[test]
        fn parse_option_after_invalid_utf8_command_name() {
            assert!(parse([c"\xff", c"--version"]).unwrap().version());
        }

        #[test]
        fn ignore_options_after_invalid_utf8_argument() {
            let options = parse([c"stak", c"\xff", c"--version", c"-s", c"42"]).unwrap();

            assert!(!options.version());
            assert_eq!(options.heap_size(), None);
            assert_eq!(options.argument_index(), 1);
        }

        #[test]
        fn reject_invalid_utf8_heap_size() {
            assert_eq!(
                parse([c"stak", c"-s", c"\xff"]).unwrap_err(),
                OptionError::HeapSize
            );
        }
    }
}
