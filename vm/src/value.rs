#[derive(Copy, Clone, Eq, Debug, PartialEq)]
pub enum Value {
    Number(u64),
    Cons(u64),
}

impl Value {
    pub const fn to_number(self) -> Option<u64> {
        if let Self::Number(x) = self {
            Some(x)
        } else {
            None
        }
    }

    pub const fn to_reference(self) -> Option<u64> {
        if let Self::Cons(x) = self {
            Some(x)
        } else {
            None
        }
    }

    #[allow(dead_code)]
    pub const fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
    }

    pub const fn is_cons(&self) -> bool {
        matches!(self, Value::Cons(_))
    }
}
