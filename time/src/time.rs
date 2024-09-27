/// A clock.
pub trait Clock {
    /// Returns a command name and its arguments in a reverse order.
    fn command_line_rev(&self) -> impl IntoIterator<Item = &str>;

    /// Returns environment variables.
    fn environment_variables(&self) -> impl IntoIterator<Item = (&str, &str)>;
}
