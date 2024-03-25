#![cfg(target_arch = "wasm32")]

use stak_configuration::DEFAULT_HEAP_SIZE;
use stak_wasm::{compile, run};
use wasm_bindgen_test::{wasm_bindgen_test, wasm_bindgen_test_configure};

wasm_bindgen_test_configure!(run_in_browser run_in_worker);

#[wasm_bindgen_test]
fn compile_source() {
    compile(
        r#"
            (import (scheme base))

            (display "Hello, World!")
        "#,
    )
    .ok()
    .unwrap();
}

#[wasm_bindgen_test]
fn run_bytecodes() {
    let bytecodes = compile(
        r#"
            (import (scheme base))

            (display "Hello, World!")
        "#,
    )
    .ok()
    .unwrap();

    run(&bytecodes, &[], DEFAULT_HEAP_SIZE).ok().unwrap();
}
