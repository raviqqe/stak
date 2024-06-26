/// A process context.
pub trait ProcessContext {
    /// Returns a command name and its arguments.
    fn command_line(&self) -> impl IntoIterator<Item = &str>;

    /// Returns environment variables.
    fn environment_variables(&self) -> impl IntoIterator<Item = (&str, &str)>;
}
