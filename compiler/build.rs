//! A build script.

use core::error::Error;
use std::{
    env,
    fs::{self, File},
    io::Write,
    path::Path,
    process::{Command, Stdio},
};

const PRELUDE_SOURCE_FILE: &str = "src/prelude.scm";
const COMPILER_SOURCE_FILE: &str = "src/compile.scm";
const COMPILER_TARGET_FILE: &str = "compile.bc";

fn main() -> Result<(), Box<dyn Error>> {
    let target_file = Path::new(&env::var("CARGO_MANIFEST_DIR")?)
        .join("src")
        .join(COMPILER_TARGET_FILE);

    if target_file.exists() {
        // Use a pre-built bytecode file.
        println!("cargo:rerun-if-changed={}", target_file.display());
        println!(
            "cargo:rustc-env=STAK_BYTECODE_FILE={}",
            target_file.display()
        );

        return Ok(());
    }

    let target_file = Path::new(&env::var("OUT_DIR")?).join(COMPILER_TARGET_FILE);

    println!("cargo:rerun-if-changed={PRELUDE_SOURCE_FILE}");
    println!("cargo:rerun-if-changed={COMPILER_SOURCE_FILE}");
    println!(
        "cargo:rustc-env=STAK_BYTECODE_FILE={}",
        target_file.display()
    );

    let file = File::options()
        .create(true)
        .write(true)
        .truncate(true)
        .open(target_file)?;
    let mut command = Command::new("stak")
        .arg(COMPILER_SOURCE_FILE)
        .stdin(Stdio::piped())
        .stdout(file)
        .spawn()?;
    let stdin = command.stdin.as_mut().expect("stdin");

    stdin.write_all(&fs::read(PRELUDE_SOURCE_FILE)?)?;
    stdin.write_all(&fs::read(COMPILER_SOURCE_FILE)?)?;

    command.wait()?;

    Ok(())
}
