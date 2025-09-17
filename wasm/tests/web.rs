#![expect(missing_docs)]
#![cfg(target_arch = "wasm32")]

use stak_configuration::DEFAULT_HEAP_SIZE;
use stak_macro::compile_r7rs;
use stak_wasm::{compile, interpret, run};
use wasm_bindgen_test::wasm_bindgen_test;

const SOURCE: &str = r#"
    (import (scheme write))

    (display "Hello, World!")
"#;

#[wasm_bindgen_test]
fn compile_source() {
    compile(SOURCE).unwrap();
}

#[wasm_bindgen_test]
fn run_bytecode() {
    const BYTECODE: &[u8] = compile_r7rs!(
        r#"
            (import (scheme write))

            (display "Hello, world!")
        "#
    );

    assert_eq!(
        interpret(BYTECODE, &[], DEFAULT_HEAP_SIZE).unwrap(),
        b"Hello, world!"
    );
}

#[wasm_bindgen_test]
fn run_script() {
    const SCRIPT: &str = r#"
        (import (scheme write))

        (display "Hello, world!")
    "#;

    assert_eq!(
        run(SCRIPT, &[], DEFAULT_HEAP_SIZE).unwrap(),
        b"Hello, world!"
    );
}

#[wasm_bindgen_test]
fn emit_error() {
    const SCRIPT: &str = r#"
        (import (scheme base))

        (error "Oh, no!")
    "#;

    assert!(format!("{:?}", run(SCRIPT, &[], DEFAULT_HEAP_SIZE).unwrap_err()).contains("Oh, no!"));
}
