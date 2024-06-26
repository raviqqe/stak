use crate::ProcessContext;

#[derive(Debug, Default)]
pub struct VoidProcessContext {}

impl VoidProcessContext {
    pub const fn new() -> Self {
        Self {}
    }
}

impl ProcessContext for VoidProcessContext {
    fn command_line(&self) -> impl IntoIterator<Item = &str> {
        []
    }

    fn environment_variables(&self) -> impl IntoIterator<Item = (&str, &str)> {
        []
    }
}
