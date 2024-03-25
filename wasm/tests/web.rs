#![cfg(target_arch = "wasm32")]

use stak_configuration::DEFAULT_HEAP_SIZE;
use stak_macro::compile_r7rs;
use stak_wasm::{compile, run};
use wasm_bindgen_test::{wasm_bindgen_test, wasm_bindgen_test_configure};

wasm_bindgen_test_configure!(run_in_browser run_in_worker);

const SOURCE: &str = r#"
    (import (scheme base))

    (display "Hello, World!")
"#;

// TODO Enable this test.
#[wasm_bindgen_test]
#[ignore]
fn compile_source() {
    compile(SOURCE).ok().unwrap();
}

// TODO Enable this test.
#[wasm_bindgen_test]
#[ignore]
fn run_bytecodes() {
    const PROGRAM: &[u8] = compile_r7rs!(
        r#"
            (import (scheme base))

            (display "Hello, World!")
        "#
    );

    run(PROGRAM, &[], DEFAULT_HEAP_SIZE).ok().unwrap();
}
