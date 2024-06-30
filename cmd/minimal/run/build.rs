//! A build script.

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
const MINIFIER_SOURCE_FILE: &str = "src/minify.scm";

fn main() -> Result<(), Box<dyn Error>> {
    build(
        MINIFIER_SOURCE_FILE,
        &[PRELUDE_SOURCE_FILE],
        "prelude.scm",
        "STAK_PRELUDE_FILE",
    )?;
    build(
        COMPILER_SOURCE_FILE,
        &[PRELUDE_SOURCE_FILE, COMPILER_SOURCE_FILE],
        "compile.bc",
        "STAK_COMPILER_FILE",
    )?;

    Ok(())
}

fn build(
    script_file: &str,
    input_files: &[&str],
    output_file: &str,
    environment_variable: &str,
) -> Result<(), Box<dyn Error>> {
    let target_file = Path::new(&env::var("OUT_DIR")?).join(output_file);

    println!(
        "cargo:rustc-env={environment_variable}={}",
        target_file.display()
    );

    let mut command = Command::new(option_env!("STAK_HOST_INTERPRETER").unwrap_or("stak"))
        .arg(script_file)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;
    let stdin = command.stdin.as_mut().expect("stdin");

    for file in input_files {
        println!("cargo:rerun-if-changed={file}");
        stdin.write_all(&fs::read(file)?)?;
    }

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
