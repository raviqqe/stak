#![expect(missing_docs)]
#![no_std]

use stak_minifier_macro::{include_minified, minify};

#[test]
fn minify_expressions() {
    const MODULE: &str = minify!("( foo  bar )\n\n(baz)");

    assert_eq!(MODULE, "(foo bar)\n(baz)\n");
}

#[test]
fn include_minified_expressions() {
    const MODULE: &str = include_minified!("../tests/foo.scm");

    assert_eq!(MODULE, "(foo bar)\n(baz)\n");
}
