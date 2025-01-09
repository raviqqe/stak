#![allow(missing_docs)]

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use stak::{
    device::{FixedBufferDevice, StdioDevice},
    file::VoidFileSystem,
    include_module,
    module::{Module, UniversalModule},
    process_context::VoidProcessContext,
    r7rs::{SmallError, SmallPrimitiveSet},
    time::VoidClock,
    vm::Vm,
};

const HEAP_SIZE: usize = 1 << 18;
const DEVICE_BUFFER_SIZE: usize = 1 << 8;

static EMPTY_MODULE: UniversalModule = include_module!("empty/main.scm");
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
        ("fibonacci", &FIBONACCI_MODULE),
        ("hello", &HELLO_MODULE),
        ("sum", &SUM_MODULE),
        ("tak", &TAK_MODULE),
    ] {
        criterion.bench_function(name, |bencher| {
            bencher.iter(|| run(black_box(module)).unwrap())
        });
    }
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(10);
    targets = stak
}
criterion_main!(benches);
