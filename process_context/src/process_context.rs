/// A process context.
pub trait ProcessContext<'a> {
    /// Returns a command name and its arguments.
    fn command_line() -> &'a [&'a str];

    /// Returns environment variables.
    fn environment_variables() -> &'a [(&'a str, &'a str)];
}
