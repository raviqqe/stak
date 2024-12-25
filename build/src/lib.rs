//! Build scripts for Stak Scheme.
//!
//! See the [`stak`](https://docs.rs/stak) crate's documentation for full examples.
//!
//! # Examples
//!
//! To build all R7RS Scheme files into bytecodes, add [the following `build.rs` build script](https://doc.rust-lang.org/cargo/reference/build-scripts.html)
//! in your crate. Then, you can include them into source files in Rust
//! using the [`stak::include_module`][include_module] macro.
//!
//! ```rust
//! use stak_build::{build_r7rs, BuildError};
//!
//! fn main() -> Result<(), BuildError> {
//! #   unsafe { std::env::set_var("OUT_DIR", "target/tmp") };
//!     build_r7rs()
//! }
//! ```
//!
//! [include_module]: https://docs.rs/stak/latest/stak/macro.include_module.html

mod error;

pub use error::BuildError;
use glob::{glob, Paths};
use stak_compiler::compile_r7rs;
use std::{
    env,
    ffi::OsStr,
    path::{Path, PathBuf},
    process::Stdio,
};
use tokio::{
    fs::{create_dir_all, read, read_to_string, write},
    process::Command,
    runtime::Runtime,
    spawn,
};
use which::which;

/// Builds R7RS Scheme source files into bytecode files.
///
/// This function builds all Scheme source files with the `.scm` file extension
/// under the `src` directory. The resulting bytecode files are stored under the
/// `target` directory.
pub fn build_r7rs() -> Result<(), BuildError> {
    let runtime = Runtime::new()?;
    let _ = runtime.enter();

    runtime.block_on(build(glob("**/*.scm")?))?;

    Ok(())
}

async fn build(paths: Paths) -> Result<(), BuildError> {
    let out_directory_variable = env::var("OUT_DIR")?;
    let out_directory = Path::new(&out_directory_variable);

    let mut handles = vec![];

    for path in paths {
        let path = path?;

        if path
            .iter()
            .any(|component| component == OsStr::new("target"))
        {
            continue;
        }

        let out_path = out_directory.join(&path);

        println!("cargo::rerun-if-changed={}", path.display());

        handles.push(spawn(compile(path, out_path)))
    }

    for handle in handles {
        handle.await??;
    }

    Ok(())
}

async fn compile(src_path: PathBuf, out_path: PathBuf) -> Result<(), BuildError> {
    let string = read_to_string(src_path).await?;
    let mut buffer = vec![];

    if which("stak-compile") {
        let mut command = Command::new("stak-compile")
            .stdin(Stdio::piped())
            .stdout(
                File::options()
                    .create(true)
                    .write(true)
                    .truncate(true)
                    .open(out_path)?,
            )
            .spawn();
        let stdin = command.stdin.as_mut().expect("stdin");

        stdin.write_all(include_str!("prelude.scm"));
        stdin.write(read(src_path).await?);
    } else {
        compile_r7rs(string.as_bytes(), &mut buffer)?;
    }

    if let Some(path) = out_path.parent() {
        create_dir_all(path).await?;
    }

    write(out_path, &buffer).await?;

    Ok(())
}
