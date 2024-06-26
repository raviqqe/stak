use crate::ProcessContext;

pub struct OsProcessContext {}

impl ProcessContext for OsProcessContext {
    fn command_line(&self) -> impl IntoIterator<Item = &str> {
        []
    }

    fn environment_variables(&self) -> impl IntoIterator<Item = (&str, &str)> {
        []
    }
}
