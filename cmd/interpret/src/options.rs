#[derive(Debug)]
pub struct Options<T> {
    version: bool,
    heap_size: Option<usize>,
    file: Option<T>,
    last_index: usize,
}

impl<T: AsRef<str>> Options<T> {
    pub fn parse(arguments: impl IntoIterator<Item = T>) -> Self {
        let mut version = false;
        let mut heap_size = None;
        let mut heap_size_option = false;
        let mut file = None;
        let mut last_index = 0;

        for (index, argument) in arguments.into_iter().enumerate().skip(1) {
            let string = argument.as_ref();
            last_index = index;

            if heap_size_option && let Ok(size) = string.parse() {
                heap_size = Some(size);
            } else if string == "--version" {
                version = true;
            } else if !matches!(string, "-s" | "--heap-size") {
                file = Some(argument);
                break;
            }

            heap_size_option = matches!(string, "-s" | "--heap-size");
        }

        Self {
            version,
            heap_size,
            file,
            last_index,
        }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_nothing() {
        let options = Options::parse([""; 0]);

        assert!(!options.version());
        assert_eq!(options.heap_size(), None);
        assert_eq!(options.file(), None);
        assert_eq!(options.last_index(), 0);
    }

    #[test]
    fn parse_command_name() {
        let options = Options::parse(["stak-interpret"]);

        assert!(!options.version());
        assert_eq!(options.heap_size(), None);
        assert_eq!(options.file(), None);
        assert_eq!(options.last_index(), 0);
    }

    #[test]
    fn parse_arguments() {
        let options = Options::parse(["stak-interpret", "foo.bc", "bar"]);

        assert!(!options.version());
        assert_eq!(options.heap_size(), None);
        assert_eq!(options.file(), Some("foo.bc"));
        assert_eq!(options.last_index(), 1);
    }

    #[test]
    fn parse_options() {
        let options =
            Options::parse(["stak-interpret", "--version", "--heap-size", "42", "foo.bc"]);

        assert!(options.version());
        assert_eq!(options.heap_size(), Some(42));
        assert_eq!(options.file(), Some("foo.bc"));
        assert_eq!(options.last_index(), 4);
    }

    #[test]
    fn parse_owned_arguments() {
        let options = Options::parse(["stak-interpret", "foo.bc"].map(String::from));

        assert_eq!(options.file(), Some("foo.bc"));
        assert_eq!(options.last_index(), 1);
    }

    mod version {
        use super::*;

        #[test]
        fn parse_option() {
            assert!(Options::parse(["stak-interpret", "--version"]).version());
        }

        #[test]
        fn parse_option_before_file() {
            assert!(Options::parse(["stak-interpret", "--version", "foo.bc"]).version());
        }

        #[test]
        fn parse_option_after_heap_size_option() {
            assert!(Options::parse(["stak-interpret", "-s", "--version"]).version());
        }

        #[test]
        fn parse_repeated_option() {
            assert!(Options::parse(["stak-interpret", "--version", "--version"]).version());
        }

        #[test]
        fn ignore_unknown_option() {
            assert!(!Options::parse(["stak-interpret", "-V"]).version());
            assert!(!Options::parse(["stak-interpret", "--versions"]).version());
        }

        #[test]
        fn ignore_option_after_file() {
            assert!(!Options::parse(["stak-interpret", "foo.bc", "--version"]).version());
        }

        #[test]
        fn ignore_option_after_unknown_option() {
            assert!(!Options::parse(["stak-interpret", "--foo", "--version"]).version());
        }

        #[test]
        fn ignore_option_after_delimiter() {
            assert!(!Options::parse(["stak-interpret", "--", "--version"]).version());
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
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "42"]).heap_size(),
                Some(42)
            );
        }

        #[test]
        fn parse_long_option() {
            assert_eq!(
                Options::parse(["stak-interpret", "--heap-size", "42"]).heap_size(),
                Some(42)
            );
        }

        #[test]
        fn parse_option_before_file() {
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "42", "foo.bc"]).heap_size(),
                Some(42)
            );
        }

        #[test]
        fn parse_option_after_missing_value() {
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "-s", "42"]).heap_size(),
                Some(42)
            );
        }

        #[test]
        fn parse_last_option() {
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "1", "--heap-size", "2"]).heap_size(),
                Some(2)
            );
        }

        #[test]
        fn parse_option_before_invalid_option() {
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "42", "--heap-size", "foo"]).heap_size(),
                Some(42)
            );
        }

        #[test]
        fn ignore_unknown_option() {
            assert_eq!(
                Options::parse(["stak-interpret", "-S", "42"]).heap_size(),
                None
            );
            assert_eq!(
                Options::parse(["stak-interpret", "--heap-sizes", "42"]).heap_size(),
                None
            );
        }

        #[test]
        fn ignore_missing_value() {
            assert_eq!(Options::parse(["stak-interpret", "-s"]).heap_size(), None);
            assert_eq!(
                Options::parse(["stak-interpret", "--heap-size"]).heap_size(),
                None
            );
        }

        #[test]
        fn ignore_invalid_value() {
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "foo"]).heap_size(),
                None
            );
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "-1"]).heap_size(),
                None
            );
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "4.2"]).heap_size(),
                None
            );
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "18446744073709551616"]).heap_size(),
                None
            );
        }

        #[test]
        fn ignore_option_with_attached_value() {
            assert_eq!(Options::parse(["stak-interpret", "-s42"]).heap_size(), None);
            assert_eq!(
                Options::parse(["stak-interpret", "-s=42"]).heap_size(),
                None
            );
            assert_eq!(
                Options::parse(["stak-interpret", "--heap-size=42"]).heap_size(),
                None
            );
        }

        #[test]
        fn ignore_option_after_file() {
            assert_eq!(
                Options::parse(["stak-interpret", "foo.bc", "-s", "42"]).heap_size(),
                None
            );
        }

        #[test]
        fn ignore_command_name() {
            assert_eq!(Options::parse(["-s", "42"]).heap_size(), None);
        }
    }

    mod file {
        use super::*;

        #[test]
        fn parse_file() {
            assert_eq!(
                Options::parse(["stak-interpret", "foo.bc"]).file(),
                Some("foo.bc")
            );
        }

        #[test]
        fn parse_file_after_version_option() {
            assert_eq!(
                Options::parse(["stak-interpret", "--version", "foo.bc"]).file(),
                Some("foo.bc")
            );
        }

        #[test]
        fn parse_file_after_heap_size_option() {
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "42", "foo.bc"]).file(),
                Some("foo.bc")
            );
        }

        #[test]
        fn parse_file_after_missing_value() {
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "foo.bc"]).file(),
                Some("foo.bc")
            );
        }

        #[test]
        fn parse_first_file() {
            assert_eq!(
                Options::parse(["stak-interpret", "foo.bc", "bar.bc"]).file(),
                Some("foo.bc")
            );
        }

        #[test]
        fn parse_empty_file() {
            assert_eq!(Options::parse(["stak-interpret", ""]).file(), Some(""));
        }

        #[test]
        fn parse_number_as_file() {
            assert_eq!(Options::parse(["stak-interpret", "42"]).file(), Some("42"));
        }

        #[test]
        fn parse_number_as_file_after_version_option() {
            assert_eq!(
                Options::parse(["stak-interpret", "--version", "42"]).file(),
                Some("42")
            );
        }

        #[test]
        fn parse_number_as_file_after_heap_size_option() {
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "42", "43"]).file(),
                Some("43")
            );
        }

        #[test]
        fn parse_number_as_file_after_missing_value() {
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "--version", "42"]).file(),
                Some("42")
            );
        }

        #[test]
        fn parse_unknown_option_as_file() {
            assert_eq!(
                Options::parse(["stak-interpret", "--foo", "bar.bc"]).file(),
                Some("--foo")
            );
        }

        #[test]
        fn parse_option_with_attached_value_as_file() {
            assert_eq!(
                Options::parse(["stak-interpret", "--heap-size=42", "foo.bc"]).file(),
                Some("--heap-size=42")
            );
        }

        #[test]
        fn parse_delimiter_as_file() {
            assert_eq!(
                Options::parse(["stak-interpret", "--", "foo.bc"]).file(),
                Some("--")
            );
        }

        #[test]
        fn ignore_options() {
            assert_eq!(
                Options::parse(["stak-interpret", "--version", "-s", "42"]).file(),
                None
            );
        }

        #[test]
        fn ignore_command_name() {
            assert_eq!(Options::parse(["foo.bc"]).file(), None);
        }
    }

    mod last_index {
        use super::*;

        #[test]
        fn parse_file() {
            assert_eq!(Options::parse(["stak-interpret", "foo.bc"]).last_index(), 1);
        }

        #[test]
        fn parse_file_after_options() {
            assert_eq!(
                Options::parse(["stak-interpret", "--version", "-s", "42", "foo.bc"]).last_index(),
                4
            );
        }

        #[test]
        fn ignore_arguments_after_file() {
            assert_eq!(
                Options::parse(["stak-interpret", "foo.bc", "bar", "baz"]).last_index(),
                1
            );
        }
    }
}
