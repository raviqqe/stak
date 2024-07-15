use crate::ProcessContext;
use core::{ffi::CStr, slice};

/// A process context based on libc.
#[derive(Debug)]
pub struct LibcProcessContext {
    arguments: &'static [*const i8],
}

impl LibcProcessContext {
    /// Creates a process context.
    pub unsafe fn new(argc: isize, argv: *const *const i8) -> Self {
        Self {
            arguments: unsafe { slice::from_raw_parts(argv, argc as _) },
        }
    }
}

impl ProcessContext for LibcProcessContext {
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
