use cfg_elif::item;

item::feature!(if ("float62") {
    mod float62;
} else if ("float") {
    mod float64;
} else {
    mod integer63;
});

item::feature!(if ("float62") {
    pub use float62::*;
} else if ("float") {
    pub use float64::*;
} else {
    pub use integer63::*;
});
