//! The pie-throwing duel.

use any_fn::{r#fn, Ref};
use core::error::Error;
use rand::random;
use stak::{
    engine::{Engine, EngineError},
    include_module,
    module::UniversalModule,
};

const HEAP_SIZE: usize = 1 << 16;

struct Person {
    pies: usize,
    dodge: f64,
    wasted: bool,
}

impl Person {
    pub fn new(pies: usize, dodge: f64) -> Self {
        Self {
            pies,
            dodge,
            wasted: false,
        }
    }

    pub fn pies(&self) -> usize {
        self.pies
    }

    pub fn wasted(&self) -> bool {
        self.wasted
    }

    pub fn throw_pie(&mut self, other: &mut Person) {
        if self.pies == 0 || self.wasted {
            return;
        }

        self.pies -= 1;

        if random::<f64>() > other.dodge {
            other.wasted = true;
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    static MODULE: UniversalModule = include_module!("fight.scm");

    run(&MODULE)?;

    Ok(())
}

fn run(module: &'static UniversalModule) -> Result<(), EngineError> {
    let mut heap = [Default::default(); HEAP_SIZE];
    let mut functions = [
        r#fn(Person::new),
        r#fn::<(Ref<_>,), _>(Person::pies),
        r#fn::<(Ref<_>,), _>(Person::wasted),
        r#fn(Person::throw_pie),
    ];
    let mut engine = Engine::new(&mut heap, &mut functions)?;

    engine.run(module)
}
