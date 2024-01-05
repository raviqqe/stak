use std::{
    error::Error,
    fs::{self, File},
    io::{self, Write},
    path::Path,
    process::{Command, Stdio},
};

const PRELUDE_SOURCE_FILE: &str = "src/prelude.scm";
const COMPILER_SOURCE_FILE: &str = "src/compile.scm";
const COMPILER_TARGET_FILE: &str = "compile.bc";

fn main() -> Result<(), Box<dyn Error>> {
    let target_file = Path::new(COMPILER_TARGET_FILE);

    // We bundle a compiler bytecode file into a crate.
    // So we want to re-build the bytecode file only if:
    //
    // - The bytecode file does not exist.
    // - Or, its source files are changed from a previous build.
    println!(
        "cargo:rustc-env=STAK_BYTECODE_FILE={}",
        target_file.display()
    );

    let target_file = Path::new("src").join(target_file);

    println!("cargo:rerun-if-changed={}", target_file.display());

    if target_file.exists() {
        return Ok(());
    }

    println!("cargo:rerun-if-changed={PRELUDE_SOURCE_FILE}");
    println!("cargo:rerun-if-changed={COMPILER_SOURCE_FILE}");

    let mut command = Command::new(option_env!("STAK_HOST_INTERPRETER").unwrap_or("gosh"))
        .arg(COMPILER_SOURCE_FILE)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;
    let stdin = command.stdin.as_mut().expect("stdin defined");

    stdin.write_all(&fs::read(PRELUDE_SOURCE_FILE)?)?;
    stdin.write_all(&fs::read(COMPILER_SOURCE_FILE)?)?;

    command.wait()?;

    io::copy(
        &mut command.stdout.expect("stdout defined"),
        &mut File::options()
            .create(true)
            .write(true)
            .truncate(true)
            .open(target_file)?,
    )?;

    Ok(())
}
