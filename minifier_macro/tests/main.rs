#![no_std]

use stak_minifier_macro::{include_minified, minify};

#[test]
fn minify_expressions() {
    const PROGRAM: &str = minify!("( foo  bar )\n\n(baz)");

    assert_eq!(PROGRAM, "(foo bar)\n(baz)\n");
}

#[test]
fn include_minified_expressions() {
    const PROGRAM: &str = include_minified!("../tests/foo.scm");

    assert_eq!(PROGRAM, "(foo bar)\n(baz)\n");
}
