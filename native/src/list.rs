use stak_vm::{Error, Memory, PrimitiveSet, Type};

/// A list primitive.
pub enum ListPrimitive {
    /// A `assq` procedure.
    Assq,
    /// A `cons` procedure.
    Cons,
    /// A `memq` procedure.
    Memq,
}

impl ListPrimitive {
    const ASSQ: usize = Self::Assq as _;
    const CONS: usize = Self::Cons as _;
    const MEMQ: usize = Self::Memq as _;
}

/// A list primitive set.
#[derive(Debug, Default)]
pub struct ListPrimitiveSet {}

impl ListPrimitiveSet {
    /// Creates a primitive set.
    pub fn new() -> Self {
        Self::default()
    }
}

impl PrimitiveSet for ListPrimitiveSet {
    type Error = Error;

    fn operate(&mut self, memory: &mut Memory, primitive: usize) -> Result<(), Self::Error> {
        match primitive {
            ListPrimitive::CONS => {
                let [car, cdr] = memory.pop_many();

                let rib = memory.allocate(car, cdr.set_tag(Type::Pair as _))?;
                memory.push(rib.into())?;
            }
            ListPrimitive::MEMQ => {
                let [x, xs] = memory.pop_many();
                let mut xs = xs.assume_cons();
                let mut y = memory.boolean(false);

                while xs != memory.null() {
                    if x == memory.car(xs) {
                        y = xs;
                        break;
                    }

                    xs = memory.cdr(xs).assume_cons();
                }

                memory.push(y.into())?;
            }
            ListPrimitive::ASSQ => {
                let [x, xs] = memory.pop_many();
                let mut xs = xs.assume_cons();
                let mut y = memory.boolean(false);

                while xs != memory.null() {
                    let cons = memory.car(xs).assume_cons();

                    if x == memory.car(cons) {
                        y = cons;
                        break;
                    }

                    xs = memory.cdr(xs).assume_cons();
                }

                memory.push(y.into())?;
            }
            _ => return Err(Error::IllegalPrimitive),
        }

        Ok(())
    }
}
