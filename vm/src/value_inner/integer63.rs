pub type NumberInner = i64;

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
pub const fn from_number(number: NumberInner) -> NumberInner {
    (number << 1) | 1
}

#[inline]
pub const fn to_number(number: NumberInner) -> NumberInner {
    number >> 1
}

#[inline]
pub const fn from_i64(number: i64) -> NumberInner {
    from_number(number)
}

#[inline]
pub const fn to_i64(number: NumberInner) -> i64 {
    to_number(number)
}

#[inline]
pub const fn from_f64(number: f64) -> NumberInner {
    from_number(number as _)
}

#[inline]
pub const fn to_f64(number: NumberInner) -> f64 {
    to_number(number) as _
}

#[inline]
pub const fn from_raw(raw: u64) -> NumberInner {
    raw as _
}

#[inline]
pub const fn to_raw(number: NumberInner) -> u64 {
    number as _
}
