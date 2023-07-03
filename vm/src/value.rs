#[derive(Copy, Clone, Eq, Debug, PartialEq)]
pub enum Value {
    Number(u64),
    Rib(u64),
}

impl Value {
    pub const fn to_raw(self) -> u64 {
        match self {
            Value::Number(number) => number,
            Value::Rib(number) => number,
        }
    }

    pub const fn is_rib(&self) -> bool {
        matches!(self, Value::Rib(_))
    }
}
