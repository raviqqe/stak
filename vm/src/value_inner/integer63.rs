pub type NumberRepresentation = i64;

#[inline]
pub fn box_cons(cons: u64) -> u64 {
    cons << 1
}

#[inline]
pub const fn unbox_cons(cons: u64) -> u64 {
    cons >> 1
}
