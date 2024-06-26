pub trait ProcessContext<'a> {
    fn command_line() -> &'a [&'a str];
    fn environment_variables() -> &'a [(&'a str, &'a str)];
}
