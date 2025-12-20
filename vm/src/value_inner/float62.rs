use libm::pow;
use nonbox::f62::{Float62, box_payload, is_payload, unbox_payload_unchecked};

pub type NumberInner = Float62;

pub const fn box_cons(cons: u64) -> u64 {
    box_payload(cons)
}

pub const fn unbox_cons(cons: u64) -> u64 {
    unbox_payload_unchecked(cons)
}

pub const fn is_cons(value: u64) -> bool {
    is_payload(value)
}

pub const fn from_number(number: NumberInner) -> NumberInner {
    number
}

pub const fn to_number(number: NumberInner) -> NumberInner {
    number
}

pub const fn from_i64(number: i64) -> NumberInner {
    Float62::from_integer(number)
}

pub const fn to_i64(number: NumberInner) -> i64 {
    let Some(integer) = number.to_integer() else {
        // Unlikely
        return number.to_float_unchecked() as _;
    };

    integer
}

pub const fn from_f64(number: f64) -> NumberInner {
    Float62::from_float(number)
}

pub const fn to_f64(number: NumberInner) -> f64 {
    let Some(integer) = number.to_integer() else {
        // Unlikely
        return number.to_float_unchecked();
    };

    integer as _
}

pub const fn from_raw(raw: u64) -> NumberInner {
    NumberInner::from_bits(raw)
}

pub const fn to_raw(number: NumberInner) -> u64 {
    number.to_bits()
}

pub fn power(x: NumberInner, y: NumberInner) -> NumberInner {
    from_f64(pow(x.to_f64(), y.to_f64()))
}
