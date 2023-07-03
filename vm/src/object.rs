#[derive(Copy, Clone, Eq, Debug, PartialEq)]
pub enum Object {
    Number(u64),
    Rib(u64),
}

impl Object {
    pub const fn to_raw(self) -> u64 {
        match self {
            Object::Number(number) => number,
            Object::Rib(number) => number,
        }
    }

    pub const fn is_rib(&self) -> bool {
        matches!(self, Object::Rib(_))
    }
}
