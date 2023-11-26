use std::{
    env,
    error::Error,
    fs::{self, File},
    io::{self, Write},
    process::{Command, Stdio},
};

const BYTECODE_FILE_VARIABLE: &str = "STAK_BYTECODE_FILE";
const COMPILER_SOURCE_FILE: &str = "compile.scm";
const COMPILER_OUT_FILE: &str = "compile.out";

fn main() -> Result<(), Box<dyn Error>> {
    println!("cargo:rerun-if-env-changed={BYTECODE_FILE_VARIABLE}");

    if let Ok(path) = env::var(BYTECODE_FILE_VARIABLE) {
        println!("cargo:rerun-if-changed={path}");
    } else {
        let source_file = format!("../{COMPILER_SOURCE_FILE}");
        let mut command = Command::new(option_env!("STAK_HOST_INTERPRETER").unwrap_or("gosh"))
            .arg(&source_file)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()?;
        let stdin = command.stdin.as_mut().unwrap();

        stdin.write_all(&fs::read("../prelude.scm")?)?;
        stdin.write_all(&fs::read(&source_file)?)?;

        command.wait()?;

        io::copy(
            &mut command.stdout.unwrap(),
            &mut File::options()
                .create(true)
                .write(true)
                .truncate(true)
                .open(COMPILER_OUT_FILE)?,
        )?;

        println!("cargo:rerun-if-changed={source_file}");
        println!("cargo:rustc-env={BYTECODE_FILE_VARIABLE}=../{COMPILER_OUT_FILE}");
    }

    Ok(())
}
