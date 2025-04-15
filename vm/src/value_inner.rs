use cfg_elif::item;

item::feature!(if ("float62") {
    mod float62;
} else if ("float") {
    mod float64;
} else {
    mod integer63;
});

item::feature!(if ("float62") {
    pub(crate) use float62::*;
} else if ("float") {
    pub(crate) use float64::*;
} else {
    pub(crate) use integer63::*;
});

#[cfg(test)]
mod tests {
    use super::*;

    const MAXIMUM_CONS: u64 = 424242;

    #[test]
    fn cons() {
        for cons in 0..MAXIMUM_CONS {
            assert_eq!(unbox_cons(box_cons(cons)), cons);
        }
    }
}
