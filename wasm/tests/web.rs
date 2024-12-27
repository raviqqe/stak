#![expect(missing_docs)]
#![cfg(target_arch = "wasm32")]

use stak_configuration::DEFAULT_HEAP_SIZE;
use stak_macro::compile_r7rs;
use stak_wasm::{compile, interpret, run};
use wasm_bindgen_test::{wasm_bindgen_test, wasm_bindgen_test_configure};

wasm_bindgen_test_configure!(run_in_browser run_in_worker);

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

            (display "Hello, World!")
        "#
    );

    assert_eq!(
        interpret(BYTECODE, &[], DEFAULT_HEAP_SIZE).ok().unwrap(),
        b"Hello, world!"
    );
}

#[wasm_bindgen_test]
fn run_script() {
    const SCRIPT: &str = r#"
        (import (scheme write))

        (display "Hello, World!")
    "#;

    assert_eq!(
        run(SCRIPT, &[], DEFAULT_HEAP_SIZE).ok().unwrap(),
        b"Hello, world!"
    );
}
