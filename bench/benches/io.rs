#![allow(missing_docs)]

use criterion::{BenchmarkId, Criterion, Throughput, black_box, criterion_group, criterion_main};
use indoc::{formatdoc, indoc};
use stak::{
    device::VoidDevice,
    file::{FileSystem, OsFileSystem, VoidFileSystem},
    process_context::VoidProcessContext,
    r7rs::{SmallError, SmallPrimitiveSet},
    time::VoidClock,
    vm::Vm,
};
use stak_compiler::compile_r7rs;
use std::{
    fs,
    path::{Path, PathBuf},
};
use tempfile::{TempDir, tempdir};

const HEAP_SIZE: usize = 1 << 22;
const SIZES: &[usize] = &[10_000, 100_000];

// Preparation groups intentionally omit throughput because they transfer no
// payload. Operation throughput is end-to-end VM/setup plus IO throughput, not
// a prep-subtracted rate.

fn compile(source: &str) -> Vec<u8> {
    let mut bytecode = vec![];

    compile_r7rs(source.as_bytes(), &mut bytecode).unwrap();

    bytecode
}

fn run(bytecode: &[u8], file_system: impl FileSystem) -> Result<(), SmallError> {
    let mut vm = Vm::new(
        vec![Default::default(); HEAP_SIZE],
        SmallPrimitiveSet::new(
            VoidDevice::new(),
            file_system,
            VoidProcessContext::new(),
            VoidClock::new(),
        ),
    )?;

    vm.run(bytecode.iter().copied())
}

fn memory_source(operation: &str, size: usize) -> Vec<u8> {
    compile(&formatdoc!(
        "
        (import (scheme base))

        (define size {size})

        {operation}
        "
    ))
}

fn file_source(operation: &str, size: usize, path: &Path) -> Vec<u8> {
    compile(&formatdoc!(
        r#"
        (import (scheme base) (scheme file))

        (define size {size})
        (define path "{}")

        {operation}
        "#,
        path.display()
    ))
}

fn create_input_file(directory: &TempDir, input: impl AsRef<[u8]>) -> PathBuf {
    let path = directory.path().join("input");
    fs::write(&path, input).unwrap();
    path
}

fn create_binary_input_file(directory: &TempDir, size: usize) -> PathBuf {
    create_input_file(directory, vec![65; size])
}

fn create_utf8_input_file(directory: &TempDir, size: usize) -> PathBuf {
    create_input_file(directory, "é".as_bytes().repeat(size))
}

fn bench_memory_ports(criterion: &mut Criterion) {
    const GROUP: &str = "io/in-memory-port";

    {
        let mut group = criterion.benchmark_group(format!("{GROUP}/prepare"));

        for &size in SIZES {
            for (name, operation) in [
                ("make-bytevector", "(make-bytevector size 65)"),
                (
                    "read/bytevector",
                    indoc!(
                        "
                        (define source (make-bytevector size 65))
                        (define port (open-input-bytevector source))
                        "
                    ),
                ),
                (
                    "write/bytevector",
                    indoc!(
                        "
                        (define source (make-bytevector size 65))
                        (define port (open-output-bytevector))
                        "
                    ),
                ),
                (
                    "read/string",
                    indoc!(
                        r"
                        (define source (make-string size #\a))
                        (define port (open-input-string source))
                        "
                    ),
                ),
                (
                    "write/string",
                    indoc!(
                        r"
                        (define source (make-string size #\a))
                        (define port (open-output-string))
                        "
                    ),
                ),
            ] {
                let bytecode = memory_source(operation, size);

                group.bench_function(BenchmarkId::new(name, size), |bencher| {
                    bencher.iter(|| {
                        run(black_box(&bytecode), VoidFileSystem::new()).unwrap();
                    })
                });
            }
        }
    }

    let mut group = criterion.benchmark_group(GROUP);

    for &size in SIZES {
        group.throughput(Throughput::Bytes(size as _));

        for (name, operation) in [
            (
                "read/bytevector",
                indoc!(
                    "
                    (define source (make-bytevector size 65))
                    (define port (open-input-bytevector source))
                    (read-bytevector size port)
                    "
                ),
            ),
            (
                "write/bytevector",
                indoc!(
                    "
                    (define source (make-bytevector size 65))
                    (define port (open-output-bytevector))
                    (write-bytevector source port)
                    (get-output-bytevector port)
                    "
                ),
            ),
            (
                "read/string",
                indoc!(
                    r"
                    (define source (make-string size #\a))
                    (define port (open-input-string source))
                    (read-string size port)
                    "
                ),
            ),
            (
                "write/string",
                indoc!(
                    r"
                    (define source (make-string size #\a))
                    (define port (open-output-string))
                    (write-string source port)
                    (get-output-string port)
                    "
                ),
            ),
        ] {
            let bytecode = memory_source(operation, size);

            group.bench_function(BenchmarkId::new(name, size), |bencher| {
                bencher.iter(|| {
                    run(black_box(&bytecode), VoidFileSystem::new()).unwrap();
                })
            });
        }
    }
}

fn bench_memory_utf8_ports(criterion: &mut Criterion) {
    const GROUP: &str = "io/in-memory-port/utf8";

    {
        let mut group = criterion.benchmark_group(format!("{GROUP}/prepare"));

        for &size in SIZES {
            for (name, operation) in [
                (
                    "read/string",
                    indoc!(
                        r"
                        (define source (make-string size #\é))
                        (define port (open-input-string source))
                        "
                    ),
                ),
                (
                    "write/string",
                    indoc!(
                        r"
                        (define source (make-string size #\é))
                        (define port (open-output-string))
                        "
                    ),
                ),
            ] {
                let bytecode = memory_source(operation, size);

                group.bench_function(BenchmarkId::new(name, size), |bencher| {
                    bencher.iter(|| {
                        run(black_box(&bytecode), VoidFileSystem::new()).unwrap();
                    })
                });
            }
        }
    }

    let mut group = criterion.benchmark_group(GROUP);

    for &size in SIZES {
        group.throughput(Throughput::Bytes((size * 2) as _));

        for (name, operation) in [
            (
                "read/string",
                indoc!(
                    r"
                    (define source (make-string size #\é))
                    (define port (open-input-string source))
                    (read-string size port)
                    "
                ),
            ),
            (
                "write/string",
                indoc!(
                    r"
                    (define source (make-string size #\é))
                    (define port (open-output-string))
                    (write-string source port)
                    (get-output-string port)
                    "
                ),
            ),
        ] {
            let bytecode = memory_source(operation, size);

            group.bench_function(BenchmarkId::new(name, size), |bencher| {
                bencher.iter(|| {
                    run(black_box(&bytecode), VoidFileSystem::new()).unwrap();
                })
            });
        }
    }
}

fn bench_os_files(criterion: &mut Criterion) {
    const GROUP: &str = "io/os-file";

    let directory = tempdir().unwrap();

    {
        let mut group = criterion.benchmark_group(format!("{GROUP}/prepare"));

        for &size in SIZES {
            for (name, operation) in [
                (
                    "read/bytevector",
                    indoc!(
                        "
                        (define port (open-input-file path))
                        (close-input-port port)
                        "
                    ),
                ),
                (
                    "read/string",
                    indoc!(
                        "
                        (define port (open-input-file path))
                        (close-input-port port)
                        "
                    ),
                ),
            ] {
                let bytecode =
                    file_source(operation, size, &create_binary_input_file(&directory, size));

                group.bench_function(BenchmarkId::new(name, size), |bencher| {
                    bencher.iter(|| {
                        run(black_box(&bytecode), OsFileSystem::new()).unwrap();
                    })
                });
            }

            for (name, operation) in [
                (
                    "write/bytevector",
                    indoc!(
                        "
                        (define source (make-bytevector size 65))
                        (define port (open-output-file path))
                        (close-output-port port)
                        "
                    ),
                ),
                (
                    "write/string",
                    indoc!(
                        r"
                        (define source (make-string size #\a))
                        (define port (open-output-file path))
                        (close-output-port port)
                        "
                    ),
                ),
            ] {
                let bytecode = file_source(operation, size, &directory.path().join("output"));

                group.bench_function(BenchmarkId::new(name, size), |bencher| {
                    bencher.iter(|| {
                        run(black_box(&bytecode), OsFileSystem::new()).unwrap();
                    })
                });
            }
        }
    }

    let mut group = criterion.benchmark_group(GROUP);

    for &size in SIZES {
        group.throughput(Throughput::Bytes(size as _));

        for (name, operation) in [
            (
                "read/bytevector",
                indoc!(
                    "
                    (define port (open-input-file path))
                    (read-bytevector size port)
                    (close-input-port port)
                    "
                ),
            ),
            (
                "read/string",
                indoc!(
                    "
                    (define port (open-input-file path))
                    (read-string size port)
                    (close-input-port port)
                    "
                ),
            ),
        ] {
            let bytecode =
                file_source(operation, size, &create_binary_input_file(&directory, size));

            group.bench_function(BenchmarkId::new(name, size), |bencher| {
                bencher.iter(|| {
                    run(black_box(&bytecode), OsFileSystem::new()).unwrap();
                })
            });
        }

        for (name, operation) in [
            (
                "write/bytevector",
                indoc!(
                    "
                    (define source (make-bytevector size 65))
                    (define port (open-output-file path))
                    (write-bytevector source port)
                    (close-output-port port)
                    "
                ),
            ),
            (
                "write/string",
                indoc!(
                    r"
                    (define source (make-string size #\a))
                    (define port (open-output-file path))
                    (write-string source port)
                    (close-output-port port)
                    "
                ),
            ),
        ] {
            let bytecode = file_source(operation, size, &directory.path().join("output"));

            group.bench_function(BenchmarkId::new(name, size), |bencher| {
                bencher.iter(|| {
                    run(black_box(&bytecode), OsFileSystem::new()).unwrap();
                })
            });
        }
    }
}

fn bench_os_utf8_files(criterion: &mut Criterion) {
    const GROUP: &str = "io/os-file/utf8";

    let directory = tempdir().unwrap();

    {
        let mut group = criterion.benchmark_group(format!("{GROUP}/prepare"));

        for &size in SIZES {
            let bytecode = file_source(
                indoc!(
                    "
                    (define port (open-input-file path))
                    (close-input-port port)
                    "
                ),
                size,
                &create_utf8_input_file(&directory, size),
            );

            group.bench_function(BenchmarkId::new("read/string", size), |bencher| {
                bencher.iter(|| {
                    run(black_box(&bytecode), OsFileSystem::new()).unwrap();
                })
            });

            let bytecode = file_source(
                indoc!(
                    r"
                    (define source (make-string size #\é))
                    (define port (open-output-file path))
                    (close-output-port port)
                    "
                ),
                size,
                &directory.path().join("output"),
            );

            group.bench_function(BenchmarkId::new("write/string", size), |bencher| {
                bencher.iter(|| {
                    run(black_box(&bytecode), OsFileSystem::new()).unwrap();
                })
            });
        }
    }

    let mut group = criterion.benchmark_group(GROUP);

    for &size in SIZES {
        group.throughput(Throughput::Bytes((size * 2) as _));

        let bytecode = file_source(
            indoc!(
                "
                (define port (open-input-file path))
                (read-string size port)
                (close-input-port port)
                "
            ),
            size,
            &create_utf8_input_file(&directory, size),
        );

        group.bench_function(BenchmarkId::new("read/string", size), |bencher| {
            bencher.iter(|| {
                run(black_box(&bytecode), OsFileSystem::new()).unwrap();
            })
        });

        let bytecode = file_source(
            indoc!(
                r"
                (define source (make-string size #\é))
                (define port (open-output-file path))
                (write-string source port)
                (close-output-port port)
                "
            ),
            size,
            &directory.path().join("output"),
        );

        group.bench_function(BenchmarkId::new("write/string", size), |bencher| {
            bencher.iter(|| {
                run(black_box(&bytecode), OsFileSystem::new()).unwrap();
            })
        });
    }
}

criterion_group!(
    benches,
    bench_memory_ports,
    bench_memory_utf8_ports,
    bench_os_files,
    bench_os_utf8_files,
);
criterion_main!(benches);
