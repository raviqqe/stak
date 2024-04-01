/// let bytecodes = stak_macro::compile_r7rs!("(define x 42)");

pub fn minify() {
    const PROGRAM: &[u8] = stak_macro::include_r7rs!("minify.scm");

    Vm::new().run(PROGRAM);
}
