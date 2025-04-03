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

static EMPTY_MODULE: UniversalModule = include_module!("empty/main.scm");
static HELLO_MODULE: UniversalModule = include_module!("hello/main.scm");

// Interesting observation here is that startup latencies get higher
// significantly as we increase heap sizes. So we should keep bytecode sizes of
// Scheme programs as small as possible to minimize the latencies.
fn run<const N: usize>(module: &'static UniversalModule) -> Result<(), SmallError> {
    let mut heap = [Default::default(); N];
    let mut vm = Vm::new(
        &mut heap,
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

fn stak_hello(bencher: &mut Bencher) {
    bencher.iter(|| {
        run::<{ 1 << 8 }>(black_box(&HELLO_MODULE)).unwrap();
    })
}

fn lua_hello(bencher: &mut Bencher) {
    bencher.iter(|| {
        Lua::new().load(black_box(r#"print("Hello, world!\n")"#)).exec().unwrap();
    })
}

fn embed(criterion: &mut Criterion) {
    for (name, benchmark) in [
        ("stak_empty", stak_empty as fn(&mut Bencher)),
        ("lua_empty", lua_empty),
        ("stak_hello", stak_hello),
        ("lua_hello", lua_hello),
    ] {
        criterion.bench_function(&format!("embed_{name}"), benchmark);
    }
}

criterion_group!(benches, embed);
criterion_main!(benches);
