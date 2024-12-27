#![expect(missing_docs)]
#![cfg(target_arch = "wasm32")]

use stak_configuration::DEFAULT_HEAP_SIZE;
use stak_macro::compile_r7rs;
use stak_wasm::{compile, interpret};
use wasm_bindgen_test::wasm_bindgen_test;

const SOURCE: &str = r#"
    (import (scheme write))

    (display "Hello, World!")
"#;

#[wasm_bindgen_test]
fn compile_source() {
    compile(SOURCE).ok().unwrap();
}

#[wasm_bindgen_test]
fn run_bytecodes() {
    const BYTECODE: &[u8] = compile_r7rs!(
        r#"
            (import (scheme write))

            (display "Hello, world!")
        "#
    );

    assert_eq!(
        interpret(BYTECODE, &[], DEFAULT_HEAP_SIZE).ok().unwrap(),
        b"Hello, world!"
    );
}
