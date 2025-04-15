pub type NumberRepresentation = i64;

#[inline]
pub const fn box_cons(cons: u64) -> u64 {
    cons << 1
}

#[inline]
pub const fn unbox_cons(cons: u64) -> u64 {
    cons >> 1
}

#[inline]
pub const fn is_cons(value: u64) -> bool {
    value & 1 == 0
}

#[inline]
pub const fn from_number(number: NumberRepresentation) -> NumberRepresentation {
    (number << 1) | 1
}

#[inline]
pub const fn to_number(number: NumberRepresentation) -> NumberRepresentation {
    number >> 1
}
