use nonbox::f62::{Float62, box_payload, is_payload, unbox_payload_unchecked};

pub type NumberRepresentation = Float62;

#[inline]
pub const fn box_cons(cons: u64) -> u64 {
    box_payload(cons)
}

#[inline]
pub const fn unbox_cons(cons: u64) -> u64 {
    unbox_payload_unchecked(cons)
}

#[inline]
pub const fn is_cons(value: u64) -> bool {
    is_payload(value)
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
    Float62::from_integer(number)
}

#[inline]
pub const fn to_i64(number: NumberRepresentation) -> i64 {
    number.to_integer_unchecked()
}

#[inline]
pub const fn from_f64(number: f64) -> NumberRepresentation {
    Float62::from_float(number)
}

#[inline]
pub const fn from_f64(number: f64) -> NumberRepresentation {
    Float62::from_float(number)
}

#[inline]
pub const fn to_f64(number: NumberRepresentation) -> f64 {
    number.to_float_unchecked()
}
