#![allow(missing_docs)]

use criterion::{BenchmarkId, Criterion, Throughput, black_box, criterion_group, criterion_main};
use stak::{
    device::VoidDevice,
    file::{OsFileSystem, VoidFileSystem},
    process_context::VoidProcessContext,
    r7rs::SmallPrimitiveSet,
    time::VoidClock,
    vm::Vm,
};
use stak_compiler::compile_r7rs;
use std::{fs, path::Path};

const HEAP_SIZE: usize = 1 << 22;
const SIZES: &[usize] = &[10_000, 100_000];

// Preparation groups intentionally omit throughput because they transfer no payload.
// Operation throughput is end-to-end VM/setup plus IO throughput, not a prep-subtracted rate.

fn compile(source: &str) -> Vec<u8> {
    let mut bytecode = vec![];

    compile_r7rs(source.as_bytes(), &mut bytecode).unwrap();

    bytecode
}

fn run<F>(bytecode: &[u8], file_system: F) -> Result<(), stak::r7rs::SmallError>
where
    F: stak::file::FileSystem,
{
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

fn scheme_string(path: &Path) -> String {
    path.to_string_lossy()
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
}

fn memory_source(operation: &str, size: usize) -> Vec<u8> {
    compile(&format!(
        r#"
(import (scheme base))

(define size {size})

{operation}
"#
    ))
}

fn file_source(operation: &str, size: usize, path: &Path) -> Vec<u8> {
    compile(&format!(
        r#"
(import (scheme base) (scheme file))

(define size {size})
(define path "{}")

{operation}
"#,
        scheme_string(path)
    ))
}

fn bench_memory_ports(criterion: &mut Criterion) {
    {
        let mut group = criterion.benchmark_group("io/in-memory-port/preparation");

        for &size in SIZES {
            for (name, operation) in [
                ("make-bytevector", "(make-bytevector size 65)"),
                (
                    "prepare/input-bytevector",
                    "(define source (make-bytevector size 65))\n(define port (open-input-bytevector source))",
                ),
                (
                    "prepare/output-bytevector",
                    "(define source (make-bytevector size 65))\n(define port (open-output-bytevector))",
                ),
                (
                    "prepare/input-string",
                    "(define source (make-string size #\\a))\n(define port (open-input-string source))",
                ),
                (
                    "prepare/output-string",
                    "(define source (make-string size #\\a))\n(define port (open-output-string))",
                ),
            ] {
                let bytecode = memory_source(operation, size);
                let id = BenchmarkId::new(name, size);

                group.bench_function(id, |bencher| {
                    bencher.iter(|| {
                        run(black_box(&bytecode), VoidFileSystem::new()).unwrap();
                    })
                });
            }
        }
    }

    let mut group = criterion.benchmark_group("io/in-memory-port");

    for &size in SIZES {
        group.throughput(Throughput::Bytes(size as u64));

        for (name, operation) in [
            (
                "read/input-bytevector",
                "(define source (make-bytevector size 65))\n(define port (open-input-bytevector source))\n(read-bytevector size port)",
            ),
            (
                "write/output-bytevector",
                "(define source (make-bytevector size 65))\n(define port (open-output-bytevector))\n(write-bytevector source port)\n(get-output-bytevector port)",
            ),
            (
                "read/input-string",
                "(define source (make-string size #\\a))\n(define port (open-input-string source))\n(read-string size port)",
            ),
            (
                "write/output-string",
                "(define source (make-string size #\\a))\n(define port (open-output-string))\n(write-string source port)\n(get-output-string port)",
            ),
        ] {
            let bytecode = memory_source(operation, size);
            let id = BenchmarkId::new(name, size);

            group.bench_function(id, |bencher| {
                bencher.iter(|| {
                    run(black_box(&bytecode), VoidFileSystem::new()).unwrap();
                })
            });
        }
    }

    group.finish();
}

fn bench_memory_utf8_ports(criterion: &mut Criterion) {
    {
        let mut group = criterion.benchmark_group("io/in-memory-port/utf8/preparation");

        for &size in SIZES {
            for (name, operation) in [
                (
                    "prepare/input-string",
                    "(define source (make-string size #\\é))\n(define port (open-input-string source))",
                ),
                (
                    "prepare/output-string",
                    "(define source (make-string size #\\é))\n(define port (open-output-string))",
                ),
            ] {
                let bytecode = memory_source(operation, size);
                let id = BenchmarkId::new(name, size);

                group.bench_function(id, |bencher| {
                    bencher.iter(|| {
                        run(black_box(&bytecode), VoidFileSystem::new()).unwrap();
                    })
                });
            }
        }
    }

    let mut group = criterion.benchmark_group("io/in-memory-port/utf8");

    for &size in SIZES {
        group.throughput(Throughput::Bytes((size * 2) as u64));

        for (name, operation) in [
            (
                "read/input-string",
                "(define source (make-string size #\\é))\n(define port (open-input-string source))\n(read-string size port)",
            ),
            (
                "write/output-string",
                "(define source (make-string size #\\é))\n(define port (open-output-string))\n(write-string source port)\n(get-output-string port)",
            ),
        ] {
            let bytecode = memory_source(operation, size);
            let id = BenchmarkId::new(name, size);

            group.bench_function(id, |bencher| {
                bencher.iter(|| {
                    run(black_box(&bytecode), VoidFileSystem::new()).unwrap();
                })
            });
        }
    }

    group.finish();
}

fn bench_os_files(criterion: &mut Criterion) {
    let directory = std::env::temp_dir().join(format!("stak-io-bench-{}", std::process::id()));
    fs::create_dir_all(&directory).unwrap();

    {
        let mut group = criterion.benchmark_group("io/os-file/preparation");

        for &size in SIZES {
            let input_path = directory.join(format!("input-{size}.bin"));
            let output_path = directory.join(format!("output-{size}.bin"));
            fs::write(&input_path, vec![65; size]).unwrap();

            for (name, operation) in [
                (
                    "prepare/input-bytevector",
                    "(define port (open-input-file path))\n(close-input-port port)",
                ),
                (
                    "prepare/input-string",
                    "(define port (open-input-file path))\n(close-input-port port)",
                ),
            ] {
                let bytecode = file_source(operation, size, &input_path);
                let id = BenchmarkId::new(name, size);

                group.bench_function(id, |bencher| {
                    bencher.iter(|| {
                        run(black_box(&bytecode), OsFileSystem::new()).unwrap();
                    })
                });
            }

            for (name, operation) in [
                (
                    "prepare/output-bytevector",
                    "(define source (make-bytevector size 65))\n(define port (open-output-file path))\n(close-output-port port)",
                ),
                (
                    "prepare/output-string",
                    "(define source (make-string size #\\a))\n(define port (open-output-file path))\n(close-output-port port)",
                ),
            ] {
                let bytecode = file_source(operation, size, &output_path);
                let id = BenchmarkId::new(name, size);

                group.bench_function(id, |bencher| {
                    bencher.iter(|| {
                        run(black_box(&bytecode), OsFileSystem::new()).unwrap();
                    })
                });
            }
        }
    }

    let mut group = criterion.benchmark_group("io/os-file");

    for &size in SIZES {
        let input_path = directory.join(format!("input-{size}.bin"));
        let output_path = directory.join(format!("output-{size}.bin"));
        assert_eq!(fs::metadata(&input_path).unwrap().len(), size as u64);
        group.throughput(Throughput::Bytes(size as u64));

        for (name, operation) in [
            (
                "read/input-bytevector",
                "(define port (open-input-file path))\n(read-bytevector size port)\n(close-input-port port)",
            ),
            (
                "read/input-string",
                "(define port (open-input-file path))\n(read-string size port)\n(close-input-port port)",
            ),
        ] {
            let bytecode = file_source(operation, size, &input_path);
            let id = BenchmarkId::new(name, size);

            group.bench_function(id, |bencher| {
                bencher.iter(|| {
                    run(black_box(&bytecode), OsFileSystem::new()).unwrap();
                })
            });
        }

        for (name, operation) in [
            (
                "write/output-bytevector",
                "(define source (make-bytevector size 65))\n(define port (open-output-file path))\n(write-bytevector source port)\n(close-output-port port)",
            ),
            (
                "write/output-string",
                "(define source (make-string size #\\a))\n(define port (open-output-file path))\n(write-string source port)\n(close-output-port port)",
            ),
        ] {
            let bytecode = file_source(operation, size, &output_path);
            let id = BenchmarkId::new(name, size);

            group.bench_function(id, |bencher| {
                bencher.iter(|| {
                    run(black_box(&bytecode), OsFileSystem::new()).unwrap();
                })
            });
        }
    }

    group.finish();

    let _ = fs::remove_dir_all(directory);
}

fn bench_os_utf8_files(criterion: &mut Criterion) {
    let directory = std::env::temp_dir().join(format!("stak-io-utf8-bench-{}", std::process::id()));
    fs::create_dir_all(&directory).unwrap();

    {
        let mut group = criterion.benchmark_group("io/os-file/utf8/preparation");

        for &size in SIZES {
            let input_path = directory.join(format!("input-{size}.bin"));
            let output_path = directory.join(format!("output-{size}.bin"));
            fs::write(&input_path, "é".as_bytes().repeat(size)).unwrap();

            let bytecode = file_source(
                "(define port (open-input-file path))\n(close-input-port port)",
                size,
                &input_path,
            );
            let id = BenchmarkId::new("prepare/input-string", size);

            group.bench_function(id, |bencher| {
                bencher.iter(|| {
                    run(black_box(&bytecode), OsFileSystem::new()).unwrap();
                })
            });

            let bytecode = file_source(
                "(define source (make-string size #\\é))\n(define port (open-output-file path))\n(close-output-port port)",
                size,
                &output_path,
            );
            let id = BenchmarkId::new("prepare/output-string", size);

            group.bench_function(id, |bencher| {
                bencher.iter(|| {
                    run(black_box(&bytecode), OsFileSystem::new()).unwrap();
                })
            });
        }
    }

    let mut group = criterion.benchmark_group("io/os-file/utf8");

    for &size in SIZES {
        let input_path = directory.join(format!("input-{size}.bin"));
        let output_path = directory.join(format!("output-{size}.bin"));
        assert_eq!(fs::metadata(&input_path).unwrap().len(), (size * 2) as u64);
        group.throughput(Throughput::Bytes((size * 2) as u64));

        let bytecode = file_source(
            "(define port (open-input-file path))\n(read-string size port)\n(close-input-port port)",
            size,
            &input_path,
        );
        let id = BenchmarkId::new("read/input-string", size);

        group.bench_function(id, |bencher| {
            bencher.iter(|| {
                run(black_box(&bytecode), OsFileSystem::new()).unwrap();
            })
        });

        let bytecode = file_source(
            "(define source (make-string size #\\é))\n(define port (open-output-file path))\n(write-string source port)\n(close-output-port port)",
            size,
            &output_path,
        );
        let id = BenchmarkId::new("write/output-string", size);

        group.bench_function(id, |bencher| {
            bencher.iter(|| {
                run(black_box(&bytecode), OsFileSystem::new()).unwrap();
            })
        });
    }

    group.finish();

    let _ = fs::remove_dir_all(directory);
}

criterion_group!(
    benches,
    bench_memory_ports,
    bench_memory_utf8_ports,
    bench_os_files,
    bench_os_utf8_files,
);
criterion_main!(benches);
