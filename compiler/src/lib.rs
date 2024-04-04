//! Stak Scheme bytecode compiler.

mod error;

pub use self::error::CompileError;
use stak_configuration::DEFAULT_HEAP_SIZE;
use stak_device::ReadWriteDevice;
use stak_primitive::SmallPrimitiveSet;
use stak_vm::Vm;
use std::{
    env,
    io::{Read, Write},
};

const PRELUDE_SOURCE: &str = include_str!("prelude.scm");
const COMPILER_BYTECODES: &[u8] = include_bytes!(env!("STAK_BYTECODE_FILE"));

/// Compiles a program in R7RS Scheme into bytecodes.
///
/// # Examples
///
/// ```rust
/// let source = "(define x 42)";
/// let mut target = vec![];
///
/// stak_compiler::compile_r7rs(source.as_bytes(), &mut target).unwrap();
/// ```
pub fn compile_r7rs(source: impl Read, target: impl Write) -> Result<(), CompileError> {
    compile_bare(PRELUDE_SOURCE.as_bytes().chain(source), target)
}

/// Compiles a program in Scheme into bytecodes with only built-ins.
///
/// # Examples
///
/// ```rust
/// let source = "($$define x 42)";
/// let mut target = vec![];
///
/// stak_compiler::compile_bare(source.as_bytes(), &mut target).unwrap();
/// ```
pub fn compile_bare(source: impl Read, target: impl Write) -> Result<(), CompileError> {
    let mut heap = vec![Default::default(); DEFAULT_HEAP_SIZE];
    let mut error_message = vec![];
    let device = ReadWriteDevice::new(source, target, &mut error_message);
    let mut vm = Vm::new(&mut heap, SmallPrimitiveSet::new(device))?;

    vm.initialize(COMPILER_BYTECODES.iter().copied())?;

    vm.run().map_err(|error| {
        if error_message.is_empty() {
            CompileError::Vm(error)
        } else {
            CompileError::User(String::from_utf8_lossy(&error_message).into_owned())
        }
    })?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    mod bare {
        use super::*;

        #[test]
        fn compile_nothing() {
            compile_bare(b"".as_slice(), &mut vec![]).unwrap();
        }

        #[test]
        fn compile_define() {
            compile_bare(b"($$define x 42)".as_slice(), &mut vec![]).unwrap();
        }
    }

    mod r7rs {
        use super::*;

        #[test]
        fn compile_nothing() {
            compile_r7rs(b"".as_slice(), &mut vec![]).unwrap();
        }

        #[test]
        fn compile_define() {
            compile_r7rs(b"(define x 42)".as_slice(), &mut vec![]).unwrap();
        }

        #[test]
        fn compile_invalid_macro_call() {
            let Err(CompileError::User(message)) = compile_r7rs(
                indoc!(
                    r#"
                    (import (scheme base))

                    (define-syntax foo
                        (syntax-rules ()
                            ((_)
                                #f)))

                    (foo 42)
                    "#
                )
                .as_bytes(),
                &mut vec![],
            ) else {
                panic!()
            };

            assert!(message.contains("invalid syntax"));
        }

        #[test]
        fn compile_write_library() {
            compile_r7rs(b"(import (scheme write))".as_slice(), &mut vec![]).unwrap();
        }
    }
}
