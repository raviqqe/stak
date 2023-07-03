#[derive(Copy, Clone, Eq, Debug, PartialEq)]
pub enum Value {
    Number(u64),
    Reference(u64),
}

impl Value {
    pub const fn to_number(self) -> u64 {
        match self {
            Value::Number(x) => x,
            Value::Reference(x) => x,
        }
    }

    pub const fn to_reference(self) -> u64 {
        match self {
            Value::Number(x) => x,
            Value::Reference(x) => x,
        }
    }

    #[allow(dead_code)]
    pub const fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
    }

    pub const fn is_cons(&self) -> bool {
        matches!(self, Value::Reference(_))
    }
}
