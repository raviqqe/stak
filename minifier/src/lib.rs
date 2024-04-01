/// Minifies given source codes.
pub fn minify(read: impl Read, writer: impl Write) -> Result<(), Box<dyn Error>> {
    const PROGRAM: &[u8] = stak_macro::include_r7rs!("minify.scm");

    let mut vm = Vm::new();

    vm.initialize(PROGRAM.iter().copied())?;
    vm.run(PROGRAM)?;

    Ok(())
}
