use crate::{cons::Cons, number::Number};

#[allow(dead_code)]
#[derive(Copy, Clone, Eq, Debug, PartialEq)]
pub enum Value {
    Number(Number),
    Cons(Cons),
}

#[allow(dead_code)]
impl Value {
    pub const fn to_number(self) -> Option<Number> {
        if let Self::Number(x) = self {
            Some(x)
        } else {
            None
        }
    }

    pub const fn to_reference(self) -> Option<Cons> {
        if let Self::Cons(x) = self {
            Some(x)
        } else {
            None
        }
    }

    pub const fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
    }

    pub const fn is_cons(&self) -> bool {
        matches!(self, Value::Cons(_))
    }
}
