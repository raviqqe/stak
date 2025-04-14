use nonbox::f62::{Float62, box_payload, unbox_payload_unchecked};

pub type NumberRepresentation = Float62;

#[inline]
pub const fn box_cons(cons: u64) -> u64 {
    box_payload(cons)
}

#[inline]
pub const fn unbox_cons(cons: u64) -> u64 {
    unbox_payload_unchecked(cons)
}
