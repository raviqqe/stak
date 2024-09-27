use crate::Time;
use core::{ffi::CStr, slice};

/// A time based on libc.
#[derive(Debug)]
pub struct LibcTime {
    arguments: &'static [*const i8],
}

impl LibcTime {
    /// Creates a time.
    ///
    /// # Safety
    ///
    /// The `argc` and `argv` arguments should be the ones passed down as
    /// arguments to the `main` function in C.
    pub const unsafe fn new(argc: isize, argv: *const *const i8) -> Self {
        Self {
            arguments: unsafe { slice::from_raw_parts(argv, argc as _) },
        }
    }
}

impl Time for LibcTime {
    fn command_line_rev(&self) -> impl IntoIterator<Item = &str> {
        self.arguments
            .iter()
            .rev()
            .map(|&argument| unsafe { CStr::from_ptr(argument) }.to_str().unwrap())
    }

    fn environment_variables(&self) -> impl IntoIterator<Item = (&str, &str)> {
        []
    }
}
