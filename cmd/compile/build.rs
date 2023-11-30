use std::{
    error::Error,
    fs::{self, File},
    io::{self, Write},
    process::{Command, Stdio},
};

const PRELUDE_SOURCE_FILE: &str = "../prelude.scm";
const COMPILER_SOURCE_FILE: &str = "../compile.scm";
const COMPILER_OUT_FILE: &str = "main.out";

fn main() -> Result<(), Box<dyn Error>> {
    println!("cargo:rerun-if-changed={COMPILER_SOURCE_FILE}");
    println!("cargo:rustc-env=STAK_BYTECODE_FILE=../{COMPILER_OUT_FILE}");

    let mut command = Command::new(option_env!("STAK_HOST_INTERPRETER").unwrap_or("gosh"))
        .arg(COMPILER_SOURCE_FILE)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;
    let stdin = command.stdin.as_mut().unwrap();

    stdin.write_all(&fs::read(PRELUDE_SOURCE_FILE)?)?;
    stdin.write_all(&fs::read(COMPILER_SOURCE_FILE)?)?;

    command.wait()?;

    io::copy(
        &mut command.stdout.unwrap(),
        &mut File::options()
            .create(true)
            .write(true)
            .truncate(true)
            .open(COMPILER_OUT_FILE)?,
    )?;

    Ok(())
}
