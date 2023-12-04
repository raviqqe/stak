use std::{
    env,
    error::Error,
    fs::{self, File},
    io::{self, Write},
    path::Path,
    process::{Command, Stdio},
};

const PRELUDE_SOURCE_FILE: &str = "prelude.scm";
const COMPILER_SOURCE_FILE: &str = "compile.scm";

fn main() -> Result<(), Box<dyn Error>> {
    let target_file = Path::new(&env::var("OUT_DIR").unwrap()).join("main.bc");

    println!("cargo:rerun-if-changed={PRELUDE_SOURCE_FILE}");
    println!("cargo:rerun-if-changed={COMPILER_SOURCE_FILE}");
    println!(
        "cargo:rustc-env=STAK_BYTECODE_FILE={}",
        target_file.display()
    );

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
            .open(target_file)?,
    )?;

    Ok(())
}
