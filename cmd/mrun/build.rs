use std::{
    env,
    error::Error,
    fs::{self, File},
    io::{self, Write},
    path::Path,
    process::{Command, Stdio},
};

const PRELUDE_SOURCE_FILE: &str = "src/prelude.scm";
const COMPILER_SOURCE_FILE: &str = "src/compile.scm";
const COMPILER_TARGET_FILE: &str = "compile.bc";

// TODO Share logic in build scripts with `stak-compiler`.
fn main() -> Result<(), Box<dyn Error>> {
    let target_file = Path::new(&env::var("OUT_DIR")?).join(COMPILER_TARGET_FILE);

    println!("cargo:rerun-if-changed={PRELUDE_SOURCE_FILE}");
    println!("cargo:rerun-if-changed={COMPILER_SOURCE_FILE}");
    println!(
        "cargo:rustc-env=STAK_BYTECODE_FILE={}",
        target_file.display()
    );

    let mut command = Command::new(option_env!("STAK_HOST_INTERPRETER").unwrap_or("stak"))
        .arg(COMPILER_SOURCE_FILE)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;
    let stdin = command.stdin.as_mut().expect("stdin");

    stdin.write_all(&fs::read(PRELUDE_SOURCE_FILE)?)?;
    stdin.write_all(&fs::read(COMPILER_SOURCE_FILE)?)?;

    command.wait()?;

    io::copy(
        &mut command.stdout.expect("stdout"),
        &mut File::options()
            .create(true)
            .write(true)
            .truncate(true)
            .open(target_file)?,
    )?;

    Ok(())
}
