use nonbox::f64::{box_unsigned, unbox_unsigned_unchecked};

pub type NumberRepresentation = f64;

#[inline]
pub const fn box_cons(cons: u64) -> u64 {
    box_unsigned(cons)
}

#[inline]
pub const fn unbox_cons(cons: u64) -> u64 {
    unbox_unsigned_unchecked(cons)
}
