#![allow(missing_docs)]

use criterion::{Bencher, Criterion, black_box, criterion_group, criterion_main};
use mlua::Lua;
use stak::{
    device::VoidDevice,
    file::VoidFileSystem,
    include_module,
    module::{Module, UniversalModule},
    process_context::VoidProcessContext,
    r7rs::{SmallError, SmallPrimitiveSet},
    time::VoidClock,
    vm::Vm,
};

static ADD_MODULE: UniversalModule = include_module!("add/main.scm");
static EMPTY_MODULE: UniversalModule = include_module!("empty/main.scm");

// Interesting observation here is that startup latencies get higher
// significantly as we increase heap sizes. So we should keep bytecode sizes of
// Scheme programs as small as possible to minimize the latencies.
fn run<const N: usize>(module: &'static UniversalModule) -> Result<(), SmallError> {
    let mut vm = Vm::new(
        [Default::default(); N],
        SmallPrimitiveSet::new(
            VoidDevice::new(),
            VoidFileSystem::new(),
            VoidProcessContext::new(),
            VoidClock::new(),
        ),
    )?;

    vm.initialize(module.bytecode().iter().copied())?;
    vm.run()
}

fn stak_add(bencher: &mut Bencher) {
    bencher.iter(|| {
        run::<{ 1 << 16 }>(black_box(&ADD_MODULE)).unwrap();
    })
}

fn lua_add(bencher: &mut Bencher) {
    bencher.iter(|| {
        Lua::new()
            .load(black_box(r#"x = 1 + 2 + 3"#))
            .exec()
            .unwrap();
    })
}

fn stak_empty(bencher: &mut Bencher) {
    bencher.iter(|| {
        run::<{ 1 << 8 }>(black_box(&EMPTY_MODULE)).unwrap();
    })
}

fn lua_empty(bencher: &mut Bencher) {
    bencher.iter(|| {
        Lua::new().load(black_box("")).exec().unwrap();
    })
}

fn embed(criterion: &mut Criterion) {
    for (name, benchmark) in [
        ("stak_add", stak_add as fn(&mut Bencher)),
        ("lua_add", lua_add),
        ("stak_empty", stak_empty),
        ("lua_empty", lua_empty),
    ] {
        criterion.bench_function(&format!("embed_{name}"), benchmark);
    }
}

criterion_group!(benches, embed);
criterion_main!(benches);
