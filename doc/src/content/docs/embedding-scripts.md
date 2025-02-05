---
title: Embedding Scheme scripts in Rust
description: How to embed Scheme scripts in Rust
---

This page explains how to embed scripts written in Stak Scheme into Rust programs. By reading this page, you will learn:

- How to add Stak Scheme as a dependency in your Rust projects.
- How to compile Scheme scripts for Rust programs to include them.
- How to embed scripts written in Stak Scheme in Rust programs.

## Preparing a Scheme script

First, prepare a Scheme script named `src/fight.scm`:

```scheme
(import (scheme base) (stak rust))

(define-rust
  make-person
  person-throw-pie
  person-wasted)

(define me (make-person 4 0.2))
(define you (make-person 2 0.6))

(person-throw-pie me you)
(person-throw-pie you me)
(person-throw-pie me you)
(person-throw-pie you me)

(when (person-wasted you)
  (write-string "Congrats!"))
(when (person-wasted me)
  (write-string "Oh, no!"))
```

## Adding a build script

Then, add a build script at `build.rs` to build the Scheme source file
into bytecodes.

```rust no_run
use stak_build::{build_r7rs, BuildError};

fn main() -> Result<(), BuildError> {
    build_r7rs()
}
```

## Embedding a Scheme script in a Rust program

Finally, you can embed and run the Scheme script in a Rust program.

```rust
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

    pub fn wasted(&self) -> bool {
        self.wasted
    }

    pub fn throw_pie(&mut self, other: &mut Person) {
        if self.wasted {
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
        r#fn(Person::throw_pie),
        r#fn::<(Ref<_>,), _>(Person::wasted),
    ];
    let mut engine = Engine::new(&mut heap, &mut functions)?;

    engine.run(module)
}
```

# References

- [`examples/embedded-script` directory on GitHub](https://github.com/raviqqe/stak/tree/main/examples/embedded-script)
