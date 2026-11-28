mod error;

#[cfg(any(not(feature = "std"), test))]
use core::{ffi::CStr, slice};
use error::OptionError;

#[derive(Debug)]
pub struct Options<T> {
    version: bool,
    heap_size: Option<usize>,
    file: Option<T>,
    last_index: usize,
}

impl<T: AsRef<str>> Options<T> {
    pub fn parse(arguments: impl IntoIterator<Item = T>) -> Result<Self, OptionError> {
        let mut version = false;
        let mut heap_size = None;
        let mut heap_size_option = false;
        let mut delimiter = false;
        let mut file = None;
        let mut last_index = 0;

        for (index, argument) in arguments.into_iter().enumerate().skip(1) {
            let string = argument.as_ref();
            last_index = index;

            if heap_size_option {
                heap_size = Some(string.parse().map_err(|_| OptionError::HeapSize)?);
            } else if delimiter {
                file = Some(argument);
                break;
            } else if string == "--" {
                delimiter = true;
            } else if string == "--version" {
                version = true;
            } else if !matches!(string, "-s" | "--heap-size") {
                file = Some(argument);
                break;
            }

            heap_size_option = matches!(string, "-s" | "--heap-size");
        }

        if heap_size_option {
            return Err(OptionError::HeapSize);
        }

        Ok(Self {
            version,
            heap_size,
            file,
            last_index,
        })
    }

    pub const fn version(&self) -> bool {
        self.version
    }

    pub const fn heap_size(&self) -> Option<usize> {
        self.heap_size
    }

    pub fn file(&self) -> Option<&str> {
        self.file.as_ref().map(AsRef::as_ref)
    }

    pub const fn last_index(&self) -> usize {
        self.last_index
    }
}

#[cfg(any(not(feature = "std"), test))]
impl Options<&'static str> {
    /// Parses command line arguments in C.
    ///
    /// # Safety
    ///
    /// The `argc` and `argv` arguments should be the ones passed down as
    /// arguments to the `main` function in C.
    pub unsafe fn parse_c(argc: isize, argv: *const *const i8) -> Result<Self, OptionError> {
        Self::parse(
            // SAFETY: Operating systems guarantee the length of `argv` to be
            // `argc`.
            unsafe { slice::from_raw_parts(argv, argc as _) }
                .iter()
                .map(|&argument| {
                    // SAFETY: Operating systems guarantee elements in `argv` to
                    // be C strings that live until the process exits.
                    unsafe { CStr::from_ptr(argument as _) }
                        .to_str()
                        .unwrap_or_default()
                }),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[cfg(not(feature = "std"))]
    use alloc::string::String;

    #[test]
    fn parse_nothing() {
        let options = Options::parse([""; 0]).unwrap();

        assert!(!options.version());
        assert_eq!(options.heap_size(), None);
        assert_eq!(options.file(), None);
        assert_eq!(options.last_index(), 0);
    }

    #[test]
    fn parse_command_name() {
        let options = Options::parse(["stak-interpret"]).unwrap();

        assert!(!options.version());
        assert_eq!(options.heap_size(), None);
        assert_eq!(options.file(), None);
        assert_eq!(options.last_index(), 0);
    }

    #[test]
    fn parse_arguments() {
        let options = Options::parse(["stak-interpret", "foo.bc", "bar"]).unwrap();

        assert!(!options.version());
        assert_eq!(options.heap_size(), None);
        assert_eq!(options.file(), Some("foo.bc"));
        assert_eq!(options.last_index(), 1);
    }

    #[test]
    fn parse_options() {
        let options =
            Options::parse(["stak-interpret", "--version", "--heap-size", "42", "foo.bc"]).unwrap();

        assert!(options.version());
        assert_eq!(options.heap_size(), Some(42));
        assert_eq!(options.file(), Some("foo.bc"));
        assert_eq!(options.last_index(), 4);
    }

    #[test]
    fn parse_owned_arguments() {
        let options = Options::parse(["stak-interpret", "foo.bc"].map(String::from)).unwrap();

        assert_eq!(options.file(), Some("foo.bc"));
        assert_eq!(options.last_index(), 1);
    }

    mod version {
        use super::*;

        #[test]
        fn parse_option() {
            assert!(
                Options::parse(["stak-interpret", "--version"])
                    .unwrap()
                    .version()
            );
        }

        #[test]
        fn parse_option_before_file() {
            assert!(
                Options::parse(["stak-interpret", "--version", "foo.bc"])
                    .unwrap()
                    .version()
            );
        }

        #[test]
        fn parse_option_before_delimiter() {
            assert!(
                Options::parse(["stak-interpret", "--version", "--"])
                    .unwrap()
                    .version()
            );
        }

        #[test]
        fn parse_repeated_option() {
            assert!(
                Options::parse(["stak-interpret", "--version", "--version"])
                    .unwrap()
                    .version()
            );
        }

        #[test]
        fn ignore_unknown_option() {
            assert!(!Options::parse(["stak-interpret", "-V"]).unwrap().version());
            assert!(
                !Options::parse(["stak-interpret", "--versions"])
                    .unwrap()
                    .version()
            );
        }

        #[test]
        fn ignore_option_after_file() {
            assert!(
                !Options::parse(["stak-interpret", "foo.bc", "--version"])
                    .unwrap()
                    .version()
            );
        }

        #[test]
        fn ignore_option_after_unknown_option() {
            assert!(
                !Options::parse(["stak-interpret", "--foo", "--version"])
                    .unwrap()
                    .version()
            );
        }

        #[test]
        fn ignore_option_after_delimiter() {
            assert!(
                !Options::parse(["stak-interpret", "--", "--version"])
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
                Options::parse(["stak-interpret", "-s", "42"])
                    .unwrap()
                    .heap_size(),
                Some(42)
            );
        }

        #[test]
        fn parse_long_option() {
            assert_eq!(
                Options::parse(["stak-interpret", "--heap-size", "42"])
                    .unwrap()
                    .heap_size(),
                Some(42)
            );
        }

        #[test]
        fn parse_option_before_file() {
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "42", "foo.bc"])
                    .unwrap()
                    .heap_size(),
                Some(42)
            );
        }

        #[test]
        fn parse_option_before_delimiter() {
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "42", "--"])
                    .unwrap()
                    .heap_size(),
                Some(42)
            );
        }

        #[test]
        fn parse_last_option() {
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "1", "--heap-size", "2"])
                    .unwrap()
                    .heap_size(),
                Some(2)
            );
        }

        #[test]
        fn ignore_unknown_option() {
            assert_eq!(
                Options::parse(["stak-interpret", "-S", "42"])
                    .unwrap()
                    .heap_size(),
                None
            );
            assert_eq!(
                Options::parse(["stak-interpret", "--heap-sizes", "42"])
                    .unwrap()
                    .heap_size(),
                None
            );
        }

        #[test]
        fn ignore_option_with_attached_value() {
            assert_eq!(
                Options::parse(["stak-interpret", "-s42"])
                    .unwrap()
                    .heap_size(),
                None
            );
            assert_eq!(
                Options::parse(["stak-interpret", "-s=42"])
                    .unwrap()
                    .heap_size(),
                None
            );
            assert_eq!(
                Options::parse(["stak-interpret", "--heap-size=42"])
                    .unwrap()
                    .heap_size(),
                None
            );
        }

        #[test]
        fn ignore_option_after_file() {
            assert_eq!(
                Options::parse(["stak-interpret", "foo.bc", "-s", "42"])
                    .unwrap()
                    .heap_size(),
                None
            );
        }

        #[test]
        fn ignore_missing_value_after_file() {
            assert_eq!(
                Options::parse(["stak-interpret", "foo.bc", "-s"])
                    .unwrap()
                    .heap_size(),
                None
            );
        }

        #[test]
        fn ignore_option_after_delimiter() {
            assert_eq!(
                Options::parse(["stak-interpret", "--", "-s", "42"])
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
                Options::parse(["stak-interpret", "-s"]).unwrap_err(),
                OptionError::HeapSize
            );
            assert_eq!(
                Options::parse(["stak-interpret", "--heap-size"]).unwrap_err(),
                OptionError::HeapSize
            );
        }

        #[test]
        fn reject_invalid_value() {
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "foo"]).unwrap_err(),
                OptionError::HeapSize
            );
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "-1"]).unwrap_err(),
                OptionError::HeapSize
            );
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "4.2"]).unwrap_err(),
                OptionError::HeapSize
            );
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "18446744073709551616"]).unwrap_err(),
                OptionError::HeapSize
            );
            assert_eq!(
                Options::parse(["stak-interpret", "-s", ""]).unwrap_err(),
                OptionError::HeapSize
            );
        }

        #[test]
        fn reject_option_as_value() {
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "--version", "42"]).unwrap_err(),
                OptionError::HeapSize
            );
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "-s", "42"]).unwrap_err(),
                OptionError::HeapSize
            );
        }

        #[test]
        fn reject_delimiter_as_value() {
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "--", "42"]).unwrap_err(),
                OptionError::HeapSize
            );
        }

        #[test]
        fn reject_missing_value_after_version_option() {
            assert_eq!(
                Options::parse(["stak-interpret", "--version", "-s"]).unwrap_err(),
                OptionError::HeapSize
            );
        }

        #[test]
        fn reject_invalid_value_after_option() {
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "42", "--heap-size", "foo"]).unwrap_err(),
                OptionError::HeapSize
            );
        }
    }

    mod file {
        use super::*;

        #[test]
        fn parse_file() {
            assert_eq!(
                Options::parse(["stak-interpret", "foo.bc"]).unwrap().file(),
                Some("foo.bc")
            );
        }

        #[test]
        fn parse_file_after_version_option() {
            assert_eq!(
                Options::parse(["stak-interpret", "--version", "foo.bc"])
                    .unwrap()
                    .file(),
                Some("foo.bc")
            );
        }

        #[test]
        fn parse_file_after_heap_size_option() {
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "42", "foo.bc"])
                    .unwrap()
                    .file(),
                Some("foo.bc")
            );
        }

        #[test]
        fn parse_first_file() {
            assert_eq!(
                Options::parse(["stak-interpret", "foo.bc", "bar.bc"])
                    .unwrap()
                    .file(),
                Some("foo.bc")
            );
        }

        #[test]
        fn parse_empty_file() {
            assert_eq!(
                Options::parse(["stak-interpret", ""]).unwrap().file(),
                Some("")
            );
        }

        #[test]
        fn parse_number_as_file() {
            assert_eq!(
                Options::parse(["stak-interpret", "42"]).unwrap().file(),
                Some("42")
            );
        }

        #[test]
        fn parse_number_as_file_after_version_option() {
            assert_eq!(
                Options::parse(["stak-interpret", "--version", "42"])
                    .unwrap()
                    .file(),
                Some("42")
            );
        }

        #[test]
        fn parse_number_as_file_after_heap_size_option() {
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "42", "43"])
                    .unwrap()
                    .file(),
                Some("43")
            );
        }

        #[test]
        fn parse_unknown_option_as_file() {
            assert_eq!(
                Options::parse(["stak-interpret", "--foo", "bar.bc"])
                    .unwrap()
                    .file(),
                Some("--foo")
            );
        }

        #[test]
        fn parse_option_with_attached_value_as_file() {
            assert_eq!(
                Options::parse(["stak-interpret", "--heap-size=42", "foo.bc"])
                    .unwrap()
                    .file(),
                Some("--heap-size=42")
            );
        }

        #[test]
        fn parse_file_after_delimiter() {
            assert_eq!(
                Options::parse(["stak-interpret", "--", "foo.bc"])
                    .unwrap()
                    .file(),
                Some("foo.bc")
            );
        }

        #[test]
        fn parse_file_before_delimiter() {
            assert_eq!(
                Options::parse(["stak-interpret", "foo.bc", "--", "bar.bc"])
                    .unwrap()
                    .file(),
                Some("foo.bc")
            );
        }

        #[test]
        fn parse_option_as_file_after_delimiter() {
            assert_eq!(
                Options::parse(["stak-interpret", "--", "--version"])
                    .unwrap()
                    .file(),
                Some("--version")
            );
        }

        #[test]
        fn parse_delimiter_as_file_after_delimiter() {
            assert_eq!(
                Options::parse(["stak-interpret", "--", "--"])
                    .unwrap()
                    .file(),
                Some("--")
            );
        }

        #[test]
        fn ignore_options() {
            assert_eq!(
                Options::parse(["stak-interpret", "--version", "-s", "42"])
                    .unwrap()
                    .file(),
                None
            );
        }

        #[test]
        fn ignore_delimiter() {
            assert_eq!(
                Options::parse(["stak-interpret", "--"]).unwrap().file(),
                None
            );
        }

        #[test]
        fn ignore_command_name() {
            assert_eq!(Options::parse(["foo.bc"]).unwrap().file(), None);
        }
    }

    mod last_index {
        use super::*;

        #[test]
        fn parse_file() {
            assert_eq!(
                Options::parse(["stak-interpret", "foo.bc"])
                    .unwrap()
                    .last_index(),
                1
            );
        }

        #[test]
        fn parse_file_after_options() {
            assert_eq!(
                Options::parse(["stak-interpret", "--version", "-s", "42", "foo.bc"])
                    .unwrap()
                    .last_index(),
                4
            );
        }

        #[test]
        fn parse_file_after_options_and_delimiter() {
            assert_eq!(
                Options::parse(["stak-interpret", "--version", "-s", "42", "--", "foo.bc"])
                    .unwrap()
                    .last_index(),
                5
            );
        }

        #[test]
        fn ignore_arguments_after_file() {
            assert_eq!(
                Options::parse(["stak-interpret", "foo.bc", "bar", "baz"])
                    .unwrap()
                    .last_index(),
                1
            );
        }
    }

    mod parse_c {
        use super::*;

        fn parse<const N: usize>(
            arguments: [&CStr; N],
        ) -> Result<Options<&'static str>, OptionError> {
            let arguments = arguments.map(|argument| argument.as_ptr().cast());

            // SAFETY: The arguments are an array of C strings.
            unsafe { Options::parse_c(N as _, arguments.as_ptr()) }
        }

        #[test]
        fn parse_nothing() {
            let options = parse([]).unwrap();

            assert!(!options.version());
            assert_eq!(options.heap_size(), None);
            assert_eq!(options.file(), None);
        }

        #[test]
        fn parse_version() {
            assert!(parse([c"stak-interpret", c"--version"]).unwrap().version());
        }

        #[test]
        fn parse_heap_size() {
            assert_eq!(
                parse([c"stak-interpret", c"-s", c"42"])
                    .unwrap()
                    .heap_size(),
                Some(42)
            );
        }

        #[test]
        fn parse_file() {
            assert_eq!(
                parse([c"stak-interpret", c"foo.bc"]).unwrap().file(),
                Some("foo.bc")
            );
        }

        #[test]
        fn parse_invalid_utf8_file() {
            assert_eq!(parse([c"stak-interpret", c"\xff"]).unwrap().last_index(), 1);
        }

        #[test]
        fn parse_options_before_invalid_utf8_file() {
            let options = parse([c"stak-interpret", c"--version", c"-s", c"42", c"\xff"]).unwrap();

            assert!(options.version());
            assert_eq!(options.heap_size(), Some(42));
            assert_eq!(options.last_index(), 4);
        }

        #[test]
        fn parse_option_after_invalid_utf8_command_name() {
            assert!(parse([c"\xff", c"--version"]).unwrap().version());
        }

        #[test]
        fn ignore_options_after_invalid_utf8_file() {
            let options = parse([c"stak-interpret", c"\xff", c"--version", c"-s", c"42"]).unwrap();

            assert!(!options.version());
            assert_eq!(options.heap_size(), None);
            assert_eq!(options.last_index(), 1);
        }

        #[test]
        fn reject_invalid_utf8_heap_size() {
            assert_eq!(
                parse([c"stak-interpret", c"-s", c"\xff"]).unwrap_err(),
                OptionError::HeapSize
            );
        }
    }
}
