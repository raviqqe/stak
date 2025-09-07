use nonbox::f64::{box_unsigned, is_boxed, unbox_unsigned_unchecked};

pub type NumberInner = f64;

pub const fn box_cons(cons: u64) -> u64 {
    box_unsigned(cons)
}

pub const fn unbox_cons(cons: u64) -> u64 {
    unbox_unsigned_unchecked(cons)
}

pub const fn is_cons(value: u64) -> bool {
    is_boxed(value)
}

pub const fn from_number(number: NumberInner) -> NumberInner {
    number
}

pub const fn to_number(number: NumberInner) -> NumberInner {
    number
}

pub const fn from_i64(number: i64) -> NumberInner {
    number as _
}

pub const fn to_i64(number: NumberInner) -> i64 {
    number as _
}

pub const fn from_f64(number: f64) -> NumberInner {
    number
}

pub const fn to_f64(number: NumberInner) -> f64 {
    number
}

pub const fn from_raw(raw: u64) -> NumberInner {
    NumberInner::from_bits(raw)
}

pub const fn to_raw(number: NumberInner) -> u64 {
    number.to_bits()
}
