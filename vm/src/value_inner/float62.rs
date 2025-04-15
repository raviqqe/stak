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
