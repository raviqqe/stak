#![allow(missing_docs)]

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use stak::{
    device::FixedBufferDevice,
    file::VoidFileSystem,
    include_module,
    module::{Module, UniversalModule},
    process_context::VoidProcessContext,
    r7rs::{SmallError, SmallPrimitiveSet},
    time::VoidClock,
    vm::Vm,
};
use stak_compiler::compile_r7rs;
use std::{fs::read, io::Sink, path::Path};

const HEAP_SIZE: usize = 1 << 18;
const DEVICE_BUFFER_SIZE: usize = 1 << 8;

static EMPTY_MODULE: UniversalModule = include_module!("empty/main.scm");
static EVAL_MODULE: UniversalModule = include_module!("eval/main.scm");
static FIBONACCI_MODULE: UniversalModule = include_module!("fibonacci/main.scm");
static HELLO_MODULE: UniversalModule = include_module!("hello/main.scm");
static SUM_MODULE: UniversalModule = include_module!("sum/main.scm");
static TAK_MODULE: UniversalModule = include_module!("tak/main.scm");

fn run(module: &'static UniversalModule) -> Result<(), SmallError> {
    let mut heap = [Default::default(); HEAP_SIZE];
    let mut vm = Vm::new(
        &mut heap,
        SmallPrimitiveSet::new(
            FixedBufferDevice::<DEVICE_BUFFER_SIZE, 0>::new(&[]),
            VoidFileSystem::new(),
            VoidProcessContext::new(),
            VoidClock::new(),
        ),
    )?;

    vm.initialize(module.bytecode().iter().copied())?;
    vm.run()
}

fn stak(criterion: &mut Criterion) {
    for (name, module) in [
        ("empty", &EMPTY_MODULE),
        ("eval", &EVAL_MODULE),
        ("fibonacci_32", &FIBONACCI_MODULE),
        ("hello", &HELLO_MODULE),
        ("sum_10000000", &SUM_MODULE),
        ("tak_16_8_0", &TAK_MODULE),
    ] {
        criterion.bench_function(name, |bencher| {
            bencher.iter(|| run(black_box(module)).unwrap())
        });
    }
}

fn stak_compiler(criterion: &mut Criterion) {
    for name in ["empty", "fibonacci", "hello", "sum", "tak"] {
        let source = read(Path::new("src").join(name).join("main.scm")).unwrap();
        let source = source.as_slice();

        criterion.bench_function(&format!("compile_{name}"), |bencher| {
            bencher.iter(|| compile_r7rs(black_box(source), Sink::default()).unwrap())
        });
    }
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(10);
    targets = stak, stak_compiler
}
criterion_main!(benches);
