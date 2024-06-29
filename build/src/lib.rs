//! Build scripts for Stak Scheme.

mod error;

pub use error::BuildError;
use futures::future::join_all;
use glob::{glob, Paths};
use stak_compiler::compile_r7rs;
use std::{
    env,
    error::Error,
    path::{Path, PathBuf},
};
use tokio::{
    fs::{canonicalize, read_to_string, write},
    runtime::Runtime,
    spawn,
};

pub fn build_r7rs() -> Result<(), Box<dyn Error>> {
    let runtime = Runtime::new()?;
    let _ = runtime.enter();

    runtime.block_on(build(glob("**/*.scm")?))?;

    Ok(())
}

async fn build(paths: Paths) -> Result<(), Box<dyn Error>> {
    let src_directory = Path::new(&env::var("CARGO_MANIFEST_DIR")?).join("src");
    let out_directory_variable = env::var("OUT_DIR")?;
    let out_directory = Path::new(&out_directory_variable);

    let mut handles = vec![];

    for path in paths {
        let path = canonicalize(path?).await?;
        let src_path = src_directory.join(&path);

        println!("cargo::rerun-if-changed={}", src_path.display());

        handles.push(spawn(compile(src_path, out_directory.join(&path))))
    }

    join_all(handles).await;

    Ok(())
}

async fn compile(src_path: PathBuf, out_path: PathBuf) -> Result<(), BuildError> {
    let string = read_to_string(src_path).await?;
    let mut buffer = vec![];

    compile_r7rs(string.as_bytes(), &mut buffer)?;

    write(out_path, &buffer).await?;

    Ok(())
}
