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
    if let Some(integer) = number.to_integer() {
        integer
    } else if let Some(float) = number.to_float() {
        float as _
    } else {
        0
    }
}

pub const fn from_f64(number: f64) -> NumberInner {
    Float62::from_float(number)
}

pub const fn to_f64(number: NumberInner) -> f64 {
    if let Some(integer) = number.to_integer() {
        integer as _
    } else if let Some(float) = number.to_float() {
        float
    } else {
        f64::NAN
    }
}

pub const fn from_raw(raw: u64) -> NumberInner {
    NumberInner::from_bits(raw)
}

pub const fn to_raw(number: NumberInner) -> u64 {
    number.to_bits()
}

pub fn power(x: NumberInner, y: NumberInner) -> NumberInner {
    let (Some(x), Some(y)) = (x.to_integer(), y.to_integer()) else {
        // Unlikely
        return from_f64(pow(to_f64(x), to_f64(y)));
    };

    if y >= 0
        && let Some(x) = x.checked_pow(y as _)
    {
        Float62::from_integer(x)
    } else {
        Float62::from_float(pow(x as _, y as _))
    }
}

pub fn checked_divide(x: NumberInner, y: NumberInner) -> Option<NumberInner> {
    Some(x / y)
}

pub fn checked_remainder(x: NumberInner, y: NumberInner) -> Option<NumberInner> {
    x.checked_rem(y)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn convert_infinity() {
        assert_eq!(to_f64(from_f64(f64::INFINITY)), f64::INFINITY);
        assert_eq!(to_f64(from_f64(f64::NEG_INFINITY)), f64::NEG_INFINITY);
        assert_eq!(to_i64(from_f64(f64::INFINITY)), f64::INFINITY as i64);
        assert_eq!(
            to_i64(from_f64(f64::NEG_INFINITY)),
            f64::NEG_INFINITY as i64
        );
    }

    #[test]
    fn convert_nan() {
        assert!(to_f64(from_f64(f64::NAN)).is_nan());
        assert_eq!(to_i64(from_f64(f64::NAN)), 0);
    }

    #[test]
    fn zero_power() {
        assert_eq!(to_f64(power(from_i64(2), from_i64(0))), 1.0);
        assert_eq!(to_f64(power(from_f64(2.0), from_i64(0))), 1.0);
        assert_eq!(to_f64(power(from_f64(2.0), from_f64(0.0))), 1.0);
    }

    #[test]
    fn non_zero_power() {
        assert_eq!(to_i64(power(from_i64(2), from_i64(3))), 8);
        assert_eq!(to_f64(power(from_f64(2.0), from_f64(3.0))), 8.0);
        assert_eq!(to_f64(power(from_f64(3.0), from_f64(5.0))), 243.0);
        assert_eq!(to_f64(power(from_i64(2), from_i64(-1))), 0.5);
    }
}
