use stak_vm::{Error, Heap, Memory, PrimitiveSet, Type};
use winter_maybe_async::maybe_async;

/// A list primitive.
pub enum ListPrimitive {
    /// An `assq` procedure.
    Assq,
    /// A `cons` procedure.
    Cons,
    /// A `memq` procedure.
    Memq,
    /// A `list-tail` procedure.
    Tail,
}

impl ListPrimitive {
    const ASSQ: usize = Self::Assq as _;
    const CONS: usize = Self::Cons as _;
    const MEMQ: usize = Self::Memq as _;
    const TAIL: usize = Self::Tail as _;
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

impl<H: Heap> PrimitiveSet<H> for ListPrimitiveSet {
    type Error = Error;

    #[maybe_async]
    fn operate(&mut self, memory: &mut Memory<H>, primitive: usize) -> Result<(), Self::Error> {
        match primitive {
            ListPrimitive::ASSQ => {
                let [x, xs] = memory.pop_many()?;
                let mut xs = xs.assume_cons();
                let mut y = memory.boolean(false)?;

                while xs != memory.null()? {
                    let cons = memory.car(xs)?.assume_cons();

                    if x == memory.car(cons)? {
                        y = cons;
                        break;
                    }

                    xs = memory.cdr(xs)?.assume_cons();
                }

                memory.push(y.into())?;
            }
            ListPrimitive::CONS => {
                let [car, cdr] = memory.pop_many()?;

                let rib = memory.allocate(car, cdr.set_tag(Type::Pair as _))?;
                memory.push(rib.into())?;
            }
            ListPrimitive::MEMQ => {
                let [x, xs] = memory.pop_many()?;
                let mut xs = xs.assume_cons();
                let mut y = memory.boolean(false)?;

                while xs != memory.null()? {
                    if x == memory.car(xs)? {
                        y = xs;
                        break;
                    }

                    xs = memory.cdr(xs)?.assume_cons();
                }

                memory.push(y.into())?;
            }
            ListPrimitive::TAIL => {
                let [mut xs, index] = memory.pop_many()?;
                let mut index = index.assume_number().to_i64() as usize;

                while index > 0 {
                    let Some(cons) = xs.to_cons() else {
                        break;
                    };
                    let cdr = memory.cdr(cons)?;

                    if cdr.tag() != Type::Pair as _ {
                        break;
                    }

                    xs = cdr;
                    index -= 1;
                }

                memory.push(xs)?;
            }
            _ => return Err(Error::IllegalPrimitive),
        }

        Ok(())
    }
}
