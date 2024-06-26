/// A process context.
pub trait ProcessContext<'a> {
    /// Returns a command name and its arguments.
    fn command_line() -> impl IntoIterator<Item = &'a str>;

    /// Returns environment variables.
    fn environment_variables() -> impl IntoIterator<Item = (&'a str, &'a str)>;
}
