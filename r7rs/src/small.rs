mod error;
mod primitive;

pub use self::error::Error;
use self::primitive::Primitive;
use core::ops::{Add, Div, Mul, Rem, Sub};
use stak_device::{Device, DevicePrimitiveSet};
use stak_file::{FilePrimitiveSet, FileSystem};
use stak_inexact::InexactPrimitiveSet;
use stak_native::{EqualPrimitiveSet, ListPrimitiveSet, TypeCheckPrimitiveSet};
use stak_process_context::{ProcessContext, ProcessContextPrimitiveSet};
use stak_time::{Clock, TimePrimitiveSet};
use stak_vm::{Memory, Number, NumberRepresentation, PrimitiveSet, Tag, Type, Value};

/// A primitive set that covers [the R7RS small](https://standards.scheme.org/corrected-r7rs/r7rs.html).
pub struct SmallPrimitiveSet<D: Device, F: FileSystem, P: ProcessContext, C: Clock> {
    device: DevicePrimitiveSet<D>,
    file: FilePrimitiveSet<F>,
    process_context: ProcessContextPrimitiveSet<P>,
    time: TimePrimitiveSet<C>,
    inexact: InexactPrimitiveSet,
    equal: EqualPrimitiveSet,
    type_check: TypeCheckPrimitiveSet,
    list: ListPrimitiveSet,
}

impl<D: Device, F: FileSystem, P: ProcessContext, C: Clock> SmallPrimitiveSet<D, F, P, C> {
    /// Creates a primitive set.
    pub fn new(device: D, file_system: F, process_context: P, clock: C) -> Self {
        Self {
            device: DevicePrimitiveSet::new(device),
            file: FilePrimitiveSet::new(file_system),
            process_context: ProcessContextPrimitiveSet::new(process_context),
            time: TimePrimitiveSet::new(clock),
            inexact: Default::default(),
            equal: Default::default(),
            type_check: Default::default(),
            list: Default::default(),
        }
    }

    /// Returns a reference to a device.
    pub fn device(&self) -> &D {
        self.device.device()
    }

    /// Returns a mutable reference to a device.
    pub fn device_mut(&mut self) -> &mut D {
        self.device.device_mut()
    }

    fn operate_comparison(
        memory: &mut Memory,
        operate: fn(NumberRepresentation, NumberRepresentation) -> bool,
    ) -> Result<(), Error> {
        let [x, y] = memory.pop_numbers();

        memory.push(
            memory
                .boolean(operate(x.to_representation(), y.to_representation()))
                .into(),
        )?;
        Ok(())
    }

    fn rib(memory: &mut Memory, car: Value, cdr: Value, tag: Tag) -> Result<(), Error> {
        let rib = memory.allocate(car, cdr.set_tag(tag))?;
        memory.push(rib.into())?;
        Ok(())
    }

    fn set_field<'a>(
        memory: &mut Memory<'a>,
        set_field: fn(&mut Memory<'a>, Value, Value),
    ) -> Result<(), Error> {
        let [x, y] = memory.pop_many();

        set_field(memory, x, y);
        memory.push(y)?;
        Ok(())
    }

    fn tag<'a>(
        memory: &mut Memory<'a>,
        field: impl Fn(&Memory<'a>, Value) -> Value,
    ) -> Result<(), Error> {
        memory.operate_top(|memory, value| {
            field(memory, value)
                .to_cons()
                .map(|cons| Number::new(cons.tag() as _))
                .unwrap_or_default()
                .into()
        })?;

        Ok(())
    }
}

impl<D: Device, F: FileSystem, P: ProcessContext, C: Clock> PrimitiveSet
    for SmallPrimitiveSet<D, F, P, C>
{
    type Error = Error;

    fn operate(&mut self, memory: &mut Memory, primitive: usize) -> Result<(), Self::Error> {
        match primitive {
            Primitive::RIB => {
                let [car, cdr, tag] = memory.pop_many();

                Self::rib(memory, car, cdr, tag.assume_number().to_i64() as _)?;
            }
            Primitive::CLOSE => {
                let closure = memory.pop();

                Self::rib(
                    memory,
                    memory.car_value(closure),
                    memory.stack().into(),
                    Type::Procedure as _,
                )?;
            }
            Primitive::IS_RIB => {
                memory.operate_top(|memory, value| memory.boolean(value.is_cons()).into())?
            }
            Primitive::CAR => memory.operate_top(Memory::car_value)?,
            Primitive::CDR => memory.operate_top(Memory::cdr_value)?,
            Primitive::TAG => Self::tag(memory, Memory::cdr_value)?,
            Primitive::SET_CAR => Self::set_field(memory, Memory::set_car_value)?,
            Primitive::SET_CDR => Self::set_field(memory, Memory::set_cdr_value)?,
            Primitive::EQUAL => {
                let [x, y] = memory.pop_many();
                memory.push(memory.boolean(x == y).into())?;
            }
            Primitive::LESS_THAN => Self::operate_comparison(memory, |x, y| x < y)?,
            Primitive::ADD => memory.operate_binary(Add::add)?,
            Primitive::SUBTRACT => memory.operate_binary(Sub::sub)?,
            Primitive::MULTIPLY => memory.operate_binary(Mul::mul)?,
            Primitive::DIVIDE => memory.operate_binary(Div::div)?,
            Primitive::REMAINDER => memory.operate_binary(Rem::rem)?,
            Primitive::EXPONENTIATION | Primitive::LOGARITHM => self
                .inexact
                .operate(memory, primitive - Primitive::EXPONENTIATION)?,
            Primitive::HALT => return Err(Error::Halt),
            Primitive::NULL | Primitive::PAIR => self
                .type_check
                .operate(memory, primitive - Primitive::NULL)?,
            Primitive::ASSQ | Primitive::CONS | Primitive::MEMQ => {
                self.list.operate(memory, primitive - Primitive::ASSQ)?
            }
            Primitive::EQV | Primitive::EQUAL_INNER => {
                self.equal.operate(memory, primitive - Primitive::EQV)?
            }
            Primitive::READ | Primitive::WRITE | Primitive::WRITE_ERROR => {
                self.device.operate(memory, primitive - Primitive::READ)?
            }
            Primitive::OPEN_FILE
            | Primitive::CLOSE_FILE
            | Primitive::READ_FILE
            | Primitive::WRITE_FILE
            | Primitive::DELETE_FILE
            | Primitive::EXISTS_FILE => self
                .file
                .operate(memory, primitive - Primitive::OPEN_FILE)?,
            Primitive::COMMAND_LINE | Primitive::ENVIRONMENT_VARIABLES => self
                .process_context
                .operate(memory, primitive - Primitive::COMMAND_LINE)?,
            Primitive::CURRENT_JIFFY => self
                .time
                .operate(memory, primitive - Primitive::CURRENT_JIFFY)?,
            _ => return Err(stak_vm::Error::IllegalPrimitive.into()),
        }

        Ok(())
    }
}
