#![allow(missing_docs)]

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use stak::{
    device::StdioDevice,
    file::VoidFileSystem,
    include_module,
    module::{Module, UniversalModule},
    process_context::VoidProcessContext,
    r7rs::{SmallError, SmallPrimitiveSet},
    time::VoidClock,
    vm::Vm,
};

const HEAP_SIZE: usize = 1 << 18;

static EMPTY_MODULE: UniversalModule = include_module!("empty/main.scm");
static FIBONACCI_MODULE: UniversalModule = include_module!("fibonacci/main.scm");

fn run(module: &'static UniversalModule) -> Result<(), SmallError> {
    let mut heap = [Default::default(); HEAP_SIZE];
    let mut vm = Vm::new(
        &mut heap,
        SmallPrimitiveSet::new(
            StdioDevice::new(),
            VoidFileSystem::new(),
            VoidProcessContext::new(),
            VoidClock::new(),
        ),
    )?;

    vm.initialize(module.bytecode().iter().copied())?;
    vm.run()
}

fn stak(criterion: &mut Criterion) {
    criterion.bench_function("empty", |bencher| {
        bencher.iter(|| run(black_box(&EMPTY_MODULE)))
    });

    criterion.bench_function("fibonacci", |bencher| {
        bencher.iter(|| run(black_box(&FIBONACCI_MODULE)))
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(10);
    targets = stak
}
criterion_main!(benches);
