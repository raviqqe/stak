#[derive(Debug)]
pub struct Options {
    version: bool,
    heap_size: Option<usize>,
    file_index: Option<usize>,
}

impl Options {
    pub fn parse(arguments: impl IntoIterator<Item = impl AsRef<str>>) -> Self {
        let mut version = false;
        let mut heap_size = None;
        let mut heap_size_option = false;
        let mut file_index = None;

        for (index, argument) in arguments.into_iter().enumerate().skip(1) {
            let argument = argument.as_ref();

            if heap_size_option && let Ok(size) = argument.parse() {
                heap_size = Some(size);
            } else if argument == "--version" {
                version = true;
            } else if !matches!(argument, "-s" | "--heap-size") {
                file_index = Some(index);
                break;
            }

            heap_size_option = matches!(argument, "-s" | "--heap-size");
        }

        Self {
            version,
            heap_size,
            file_index,
        }
    }

    pub const fn version(&self) -> bool {
        self.version
    }

    pub const fn heap_size(&self) -> Option<usize> {
        self.heap_size
    }

    pub const fn file_index(&self) -> Option<usize> {
        self.file_index
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
        assert_eq!(options.file_index(), None);
    }

    #[test]
    fn parse_command_name() {
        let options = Options::parse(["stak-interpret"]);

        assert!(!options.version());
        assert_eq!(options.heap_size(), None);
        assert_eq!(options.file_index(), None);
    }

    #[test]
    fn parse_arguments() {
        let options = Options::parse(["stak-interpret", "foo.bc", "bar"]);

        assert!(!options.version());
        assert_eq!(options.heap_size(), None);
        assert_eq!(options.file_index(), Some(1));
    }

    #[test]
    fn parse_options() {
        let options =
            Options::parse(["stak-interpret", "--version", "--heap-size", "42", "foo.bc"]);

        assert!(options.version());
        assert_eq!(options.heap_size(), Some(42));
        assert_eq!(options.file_index(), Some(4));
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
        fn parse_option_after_version_option() {
            assert_eq!(
                Options::parse(["stak-interpret", "--version", "-s", "42"]).heap_size(),
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
        fn ignore_option_after_delimiter() {
            assert_eq!(
                Options::parse(["stak-interpret", "--", "-s", "42"]).heap_size(),
                None
            );
        }

        #[test]
        fn ignore_command_name() {
            assert_eq!(Options::parse(["-s", "42"]).heap_size(), None);
        }
    }

    mod file_index {
        use super::*;

        #[test]
        fn parse_file() {
            assert_eq!(
                Options::parse(["stak-interpret", "foo.bc"]).file_index(),
                Some(1)
            );
        }

        #[test]
        fn parse_file_after_version_option() {
            assert_eq!(
                Options::parse(["stak-interpret", "--version", "foo.bc"]).file_index(),
                Some(2)
            );
        }

        #[test]
        fn parse_file_after_heap_size_option() {
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "42", "foo.bc"]).file_index(),
                Some(3)
            );
        }

        #[test]
        fn parse_file_after_missing_value() {
            assert_eq!(
                Options::parse(["stak-interpret", "-s", "foo.bc"]).file_index(),
                Some(2)
            );
        }

        #[test]
        fn parse_file_before_options() {
            assert_eq!(
                Options::parse(["stak-interpret", "foo.bc", "--version", "-s", "42"]).file_index(),
                Some(1)
            );
        }

        #[test]
        fn parse_first_file() {
            assert_eq!(
                Options::parse(["stak-interpret", "foo.bc", "bar.bc"]).file_index(),
                Some(1)
            );
        }

        #[test]
        fn parse_empty_file() {
            assert_eq!(Options::parse(["stak-interpret", ""]).file_index(), Some(1));
        }

        #[test]
        fn parse_number_as_file() {
            assert_eq!(
                Options::parse(["stak-interpret", "42"]).file_index(),
                Some(1)
            );
        }

        #[test]
        fn parse_unknown_option_as_file() {
            assert_eq!(
                Options::parse(["stak-interpret", "--foo", "bar.bc"]).file_index(),
                Some(1)
            );
        }

        #[test]
        fn parse_delimiter_as_file() {
            assert_eq!(
                Options::parse(["stak-interpret", "--", "foo.bc"]).file_index(),
                Some(1)
            );
        }

        #[test]
        fn ignore_options() {
            assert_eq!(
                Options::parse(["stak-interpret", "--version", "-s", "42"]).file_index(),
                None
            );
        }

        #[test]
        fn ignore_command_name() {
            assert_eq!(Options::parse(["foo.bc"]).file_index(), None);
        }
    }
}
