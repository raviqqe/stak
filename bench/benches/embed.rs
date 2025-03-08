#![allow(missing_docs)]

use criterion::{Bencher, Criterion, black_box, criterion_group, criterion_main};
use mlua::Lua;
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

const HEAP_SIZE: usize = 1 << 18;
const DEVICE_BUFFER_SIZE: usize = 1 << 8;

static EMPTY_MODULE: UniversalModule = include_module!("empty/main.scm");

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

fn stak_empty(bencher: &mut Bencher) {
    bencher.iter(|| {
        run(black_box(&EMPTY_MODULE)).unwrap();
    })
}

fn lua_empty(bencher: &mut Bencher) {
    bencher.iter(|| {
        Lua::new().load(black_box("")).exec().unwrap();
    })
}

fn embed(criterion: &mut Criterion) {
    for (name, benchmark) in [
        ("stak_empty", stak_empty as fn(&mut Bencher)),
        ("lua_empty", lua_empty),
    ] {
        criterion.bench_function(&format!("embed_{name}"), benchmark);
    }
}

criterion_group!(benches, embed);

criterion_main!(benches);
