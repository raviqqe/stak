use nonbox::f64::{box_unsigned, is_boxed, unbox_unsigned_unchecked};

pub type NumberRepresentation = f64;

#[inline]
pub const fn box_cons(cons: u64) -> u64 {
    box_unsigned(cons)
}

#[inline]
pub const fn unbox_cons(cons: u64) -> u64 {
    unbox_unsigned_unchecked(cons)
}

#[inline]
pub const fn is_cons(value: u64) -> bool {
    is_boxed(value)
}

#[inline]
pub const fn from_number(number: NumberRepresentation) -> NumberRepresentation {
    number
}

#[inline]
pub const fn to_number(number: NumberRepresentation) -> NumberRepresentation {
    number
}

#[inline]
pub const fn from_i64(number: i64) -> NumberRepresentation {
    number as _
}

#[inline]
pub const fn to_i64(number: NumberRepresentation) -> i64 {
    number as _
}
