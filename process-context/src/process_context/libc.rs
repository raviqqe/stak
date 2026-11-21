use crate::ProcessContext;
use core::{
    ffi::{CStr, c_char},
    slice,
};

unsafe extern "C" {
    static mut environ: *const *const c_char;
}

/// A process context based on libc.
#[derive(Debug)]
pub struct LibcProcessContext {
    arguments: &'static [*const i8],
}

impl LibcProcessContext {
    /// Creates a process context.
    ///
    /// # Safety
    ///
    /// The `argc` and `argv` arguments should be the ones passed down as
    /// arguments to the `main` function in C. Environment variables should not
    /// be modified while the process context is alive.
    pub const unsafe fn new(argc: isize, argv: *const *const i8) -> Self {
        Self {
            // SAFETY: Operating systems guarantee the length of `argv` to be `argc`.
            arguments: unsafe { slice::from_raw_parts(argv, argc as _) },
        }
    }
}

impl ProcessContext for LibcProcessContext {
    fn command_line_rev(&self) -> impl IntoIterator<Item = &str> {
        self.arguments.iter().rev().map(|&argument| {
            // SAFETY: Operating systems guarantee elements in `argv` to be C strings.
            unsafe { CStr::from_ptr(argument as _) }.to_str().unwrap()
        })
    }

    fn environment_variables(&self) -> impl IntoIterator<Item = (&str, &str)> {
        // SAFETY: Operating systems guarantee `environ` to be null or a
        // null-terminated array of C strings. It is not modified while the
        // process context is alive.
        unsafe { decode_environment(environ) }
    }
}

/// Decodes environment variables.
///
/// # Safety
///
/// The `environment` argument should be null or a null-terminated array of C
/// strings. It should not be modified while the environment variables are
/// alive.
unsafe fn decode_environment<'a>(
    environment: *const *const c_char,
) -> impl Iterator<Item = (&'a str, &'a str)> {
    (0..)
        .map_while(move |index| {
            // SAFETY: The environment is null or a null-terminated array. The
            // fused iterator never reads entries after its terminator.
            unsafe { environment.add(index).as_ref() }.filter(|entry| !entry.is_null())
        })
        .fuse()
        .filter_map(|&entry| {
            // SAFETY: Entries in the environment are C strings.
            unsafe { CStr::from_ptr(entry) }
                .to_str()
                .ok()?
                .split_once('=')
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec::Vec;
    use core::ptr::{dangling, null};
    use std::env::vars_os;

    #[test]
    fn environment_variables() {
        assert_eq!(
            // SAFETY: No argument is read, and no test modifies environment
            // variables.
            unsafe { LibcProcessContext::new(0, dangling()) }
                .environment_variables()
                .into_iter()
                .collect::<Vec<_>>(),
            vars_os()
                .collect::<Vec<_>>()
                .iter()
                .filter_map(|(key, value)| Some((key.to_str()?, value.to_str()?)))
                .collect::<Vec<_>>()
        );
    }

    mod decode_environment {
        use super::*;

        fn decode<'a>(entries: &[&'a CStr]) -> Vec<(&'a str, &'a str)> {
            let environment = entries
                .iter()
                .map(|entry| entry.as_ptr())
                .chain([null()])
                .collect::<Vec<_>>();

            // SAFETY: The environment is a null-terminated array of C strings.
            unsafe { decode_environment(environment.as_ptr()) }.collect()
        }

        #[test]
        fn decode_null() {
            // SAFETY: The environment is null.
            assert_eq!(unsafe { decode_environment(null()) }.next(), None);
        }

        #[test]
        fn decode_nothing() {
            assert_eq!(decode(&[]), []);
        }

        #[test]
        fn decode_variable() {
            assert_eq!(decode(&[c"FOO=bar"]), [("FOO", "bar")]);
        }

        #[test]
        fn decode_variables() {
            assert_eq!(
                decode(&[c"FOO=bar", c"BAZ=qux"]),
                [("FOO", "bar"), ("BAZ", "qux")]
            );
        }

        #[test]
        fn decode_empty_value() {
            assert_eq!(decode(&[c"FOO="]), [("FOO", "")]);
        }

        #[test]
        fn decode_value_with_equal_sign() {
            assert_eq!(decode(&[c"FOO=bar=baz"]), [("FOO", "bar=baz")]);
        }

        #[test]
        fn decode_unicode() {
            assert_eq!(decode(&[c"あ=阿😄"]), [("あ", "阿😄")]);
        }

        #[test]
        fn skip_entry_without_equal_sign() {
            assert_eq!(
                decode(&[c"FOO=bar", c"", c"BAZ", c"qux=quux"]),
                [("FOO", "bar"), ("qux", "quux")]
            );
        }

        #[test]
        fn skip_invalid_utf8() {
            assert_eq!(
                decode(&[c"FOO=bar", c"\xff=baz", c"BAZ=\xff", c"qux=quux"]),
                [("FOO", "bar"), ("qux", "quux")]
            );
        }

        #[test]
        fn stop_at_terminator() {
            let environment = [c"FOO=bar".as_ptr(), null(), c"BAZ=qux".as_ptr(), null()];
            // SAFETY: The environment is a null-terminated array of C strings.
            let mut variables = unsafe { decode_environment(environment.as_ptr()) };

            assert_eq!(variables.next(), Some(("FOO", "bar")));
            assert_eq!(variables.next(), None);
            assert_eq!(variables.next(), None);
        }
    }
}
