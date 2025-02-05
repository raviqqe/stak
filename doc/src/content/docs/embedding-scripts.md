---
title: Embedding Scheme scripts in Rust
description: How to embed Scheme scripts in Rust
---

This page explains how to embed scripts written in Stak Scheme into Rust programs. By reading this page, you will learn:

- How to compile Scheme scripts so that Rust programs can include them.
- How to include scripts written in Stak Scheme in Rust programs.

## Installing dependencies

First, follow [the install instruction](install) to add Stak Scheme as dependencies in your Rust crate.

In addition, to use native functions written in Rust in Scheme scripts later, install [the `any-fn` crate](https://crates.io/crates/any-fn) which converts statically-typed functions in Rust into dynamically-typed functions as a dependency.

## Preparing a Scheme script

First, prepare a Scheme script named `src/fight.scm` as follows.

```scheme
; Import the special library named `(stak rust)`.
(import (scheme base) (stak rust))

; Use the `define-rust` procedure to import native functions written in Rust.
; The order of the functions should match the ones passed into the `Engine::new()`
; function in Rust.
(define-rust
  make-person
  person-throw-pie
  person-wasted)

; Make two people with a number of pies they have and their dodge rates.
(define me (make-person 4 0.2))
(define friend (make-person 2 0.6))

; The fight begins. Let's throw pies to each other!
(person-throw-pie me friend)
(person-throw-pie friend me)
(person-throw-pie me friend)
(person-throw-pie friend me)

; Output the winner.
(when (person-wasted friend)
  (write-string "Congrats!"))
(when (person-wasted me)
  (write-string "Oh, no!"))
```

The main part for Rust integration is importing the `(stak rust)` library and defining Rust native functions using the `define-rust` procedure.

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

// Define a person data structure and its associated functions which you include
// into the Scheme script.
struct Person {
    pies: usize,
    dodge: f64,
    wasted: bool,
}

impl Person {
    /// Creates a person.
    pub fn new(pies: usize, dodge: f64) -> Self {
        Self {
            pies,
            dodge,
            wasted: false,
        }
    }

    /// Returns `true` if a person is wasted.
    pub fn wasted(&self) -> bool {
        self.wasted
    }

    /// Throws a pie to another person.
    pub fn throw_pie(&mut self, other: &mut Person) {
        if self.wasted {
            return;
        }

        self.pies -= 1;

        // The other person dodges a pie at a certain rate.
        if random::<f64>() > other.dodge {
            other.wasted = true;
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    // Import a Scheme module of the script.
    static MODULE: UniversalModule = include_module!("fight.scm");

    // Run the Scheme module.
    run(&MODULE)?;

    Ok(())
}

fn run(module: &'static UniversalModule) -> Result<(), EngineError> {
    // Initialize a heap memory for a Scheme scripting engine.
    let mut heap = [Default::default(); HEAP_SIZE];
    // Define Rust functions to pass to the engine.
    let mut functions = [
        r#fn(Person::new),
        r#fn(Person::throw_pie),
        r#fn::<(Ref<_>,), _>(Person::wasted),
    ];
    // Initialize the engine.
    let mut engine = Engine::new(&mut heap, &mut functions)?;

    // Finally, run the module!
    engine.run(module)
}
```

# References

- [`examples/embedded-script` directory on GitHub](https://github.com/raviqqe/stak/tree/main/examples/embedded-script)
