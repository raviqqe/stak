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
    sync::Arc,
};
use tokio::{
    fs::{create_dir_all, read, read_to_string, write},
    io::{AsyncReadExt, AsyncWriteExt},
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
    let compiler = which("stak-compile").ok().map(Arc::new);

    if compiler.is_none() {
        println!("cargo::warning={}",
            [
                "Using an internal compiler for Stak Scheme.",
                "This can be very slow unless you modify `profile.<profile>.build-override` in your `Cargo.toml` file to set `opt-level = 3`. ",
                "For more information, see https://doc.rust-lang.org/cargo/reference/profiles.html#build-dependencies.",
                "Consider installing the external compiler by running `cargo install stak-compile`.",
            ].join(" ")
        );
    }

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

        handles.push(spawn(compile(path, out_path, compiler.clone())))
    }

    for handle in handles {
        handle.await??;
    }

    Ok(())
}

async fn compile(
    src_path: PathBuf,
    out_path: PathBuf,
    compiler: Option<Arc<PathBuf>>,
) -> Result<(), BuildError> {
    let mut buffer = vec![];

    if let Some(path) = compiler {
        let mut command = Command::new(&*path)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()?;
        let stdin = command.stdin.as_mut().expect("stdin");

        stdin
            .write_all(include_str!("prelude.scm").as_bytes())
            .await?;
        stdin.write_all(&read(src_path).await?).await?;

        command.wait().await?;

        command
            .stdout
            .expect("stdout")
            .read_to_end(&mut buffer)
            .await?;
    } else {
        compile_r7rs(read_to_string(&src_path).await?.as_bytes(), &mut buffer)?;
    }

    if let Some(path) = out_path.parent() {
        create_dir_all(path).await?;
    }

    write(out_path, &buffer).await?;

    Ok(())
}
