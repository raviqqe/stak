#![expect(missing_docs)]
#![no_std]

use stak_minifier_macro::{include_minified, minify};

#[test]
fn minify_expressions() {
    const SCRIPT: &str = minify!("( foo  bar )\n\n(baz)");

    assert_eq!(SCRIPT, "(foo bar)\n(baz)\n");
}

#[test]
fn include_minified_expressions() {
    const SCRIPT: &str = include_minified!("../tests/foo.scm");

    assert_eq!(SCRIPT, "(foo bar)\n(baz)\n");
}
