mod error;
mod primitive;

pub use self::error::Error;
use self::primitive::Primitive;
use core::ops::{Add, Div, Mul, Sub};
use heapless::Vec;
use stak_device::Device;
use stak_file::FileSystem;
use stak_process_context::ProcessContext;
use stak_vm::{Memory, Number, PrimitiveSet, Tag, Type, Value};

const PATH_SIZE: usize = 64;

/// A primitive set that covers R7RS small.
pub struct SmallPrimitiveSet<D: Device, F: FileSystem, P: ProcessContext> {
    device: D,
    file_system: F,
    // TODO
    #[allow(unused)]
    process_context: P,
}

impl<D: Device, F: FileSystem, P: ProcessContext> SmallPrimitiveSet<D, F, P> {
    /// Creates a primitive set.
    pub fn new(device: D, file_system: F, process_context: P) -> Self {
        Self {
            device,
            file_system,
            process_context,
        }
    }

    /// Returns a reference to a device.
    pub fn device(&self) -> &D {
        &self.device
    }

    /// Returns a mutable reference to a device.
    pub fn device_mut(&mut self) -> &mut D {
        &mut self.device
    }

    fn operate_top<'a>(
        memory: &mut Memory<'a>,
        operate: impl Fn(&Memory<'a>, Value) -> Value,
    ) -> Result<(), Error> {
        let x = memory.pop();
        memory.push(operate(memory, x))?;
        Ok(())
    }

    fn operate_binary(memory: &mut Memory, operate: fn(i64, i64) -> i64) -> Result<(), Error> {
        let [x, y] = Self::pop_number_arguments(memory);

        memory.push(Number::new(operate(x.to_i64(), y.to_i64())).into())?;
        Ok(())
    }

    fn operate_comparison(memory: &mut Memory, operate: fn(i64, i64) -> bool) -> Result<(), Error> {
        let [x, y] = Self::pop_number_arguments(memory);

        memory.push(memory.boolean(operate(x.to_i64(), y.to_i64())).into())?;
        Ok(())
    }

    fn operate_option<'a>(
        memory: &mut Memory<'a>,
        operate: impl Fn(&mut Memory<'a>) -> Option<Value>,
    ) -> Result<(), Error> {
        let value = operate(memory).unwrap_or_else(|| memory.boolean(false).into());
        memory.push(value)?;
        Ok(())
    }

    fn operate_result<'a, E>(
        memory: &mut Memory<'a>,
        operate: impl Fn(&mut Memory<'a>) -> Result<(), E>,
    ) -> Result<(), Error> {
        let result = operate(memory);
        memory.push(memory.boolean(result.is_ok()).into())?;
        Ok(())
    }

    fn rib(memory: &mut Memory, r#type: Tag, car: Value, cdr: Value) -> Result<(), Error> {
        let rib = memory.allocate(car.set_tag(r#type), cdr)?;
        memory.push(rib.into())?;
        Ok(())
    }

    fn set_field<'a>(
        memory: &mut Memory<'a>,
        set_field: fn(&mut Memory<'a>, Value, Value),
    ) -> Result<(), Error> {
        let [x, y] = Self::pop_arguments(memory);

        set_field(memory, x, y);
        memory.push(y)?;
        Ok(())
    }

    fn tag<'a>(
        memory: &mut Memory<'a>,
        field: impl Fn(&Memory<'a>, Value) -> Value,
    ) -> Result<(), Error> {
        Self::operate_top(memory, |vm, value| {
            Number::new(
                field(vm, value)
                    .to_cons()
                    .map(|cons| cons.tag() as _)
                    .unwrap_or(Type::default() as _),
            )
            .into()
        })
    }

    fn write(
        &mut self,
        memory: &mut Memory,
        write: fn(&mut D, u8) -> Result<(), <D as Device>::Error>,
        error: Error,
    ) -> Result<(), Error> {
        let byte = memory.top().assume_number().to_i64() as u8;

        write(&mut self.device, byte).map_err(|_| error)
    }

    fn check_type(memory: &mut Memory, r#type: Type) -> Result<(), Error> {
        Self::operate_top(memory, |memory, value| {
            memory
                .boolean(
                    value
                        .to_cons()
                        .map(|cons| memory.car(cons).tag() == r#type as Tag)
                        .unwrap_or_default(),
                )
                .into()
        })
    }

    fn pop_number_arguments<const M: usize>(memory: &mut Memory) -> [Number; M] {
        let mut numbers = [Default::default(); M];

        for (index, value) in Self::pop_arguments::<M>(memory).into_iter().enumerate() {
            numbers[index] = value.assume_number();
        }

        numbers
    }

    fn pop_arguments<const M: usize>(memory: &mut Memory) -> [Value; M] {
        let mut values = [Default::default(); M];

        for index in 0..M - 1 {
            values[M - 1 - index] = memory.pop();
        }

        values[0] = memory.pop();

        values
    }

    fn decode_path(memory: &mut Memory, mut list: Value) -> Option<Vec<u8, PATH_SIZE>> {
        let mut path = Vec::<_, PATH_SIZE>::new();

        while list.assume_cons() != memory.null() {
            path.push(memory.car_value(list).assume_number().to_i64() as u8)
                .ok()?;
            list = memory.cdr_value(list);
        }

        path.push(0).ok()?;

        Some(path)
    }
}

impl<D: Device, F: FileSystem, P: ProcessContext> PrimitiveSet for SmallPrimitiveSet<D, F, P> {
    type Error = Error;

    fn operate(&mut self, memory: &mut Memory, primitive: u8) -> Result<(), Self::Error> {
        match primitive {
            Primitive::RIB => {
                let [r#type, car, cdr, tag] = Self::pop_arguments(memory);

                Self::rib(
                    memory,
                    r#type.assume_number().to_i64() as Tag,
                    car,
                    cdr.set_tag(tag.assume_number().to_i64() as Tag),
                )?;
            }
            // Optimize a cons.
            Primitive::CONS => {
                let [car, cdr] = Self::pop_arguments(memory);

                Self::rib(memory, Type::Pair as Tag, car, cdr)?;
            }
            Primitive::CLOSE => {
                let closure = memory.pop();

                Self::rib(
                    memory,
                    Type::Procedure as Tag,
                    memory.stack().into(),
                    memory.cdr_value(closure),
                )?;
            }
            Primitive::IS_RIB => Self::operate_top(memory, |memory, value| {
                memory.boolean(value.is_cons()).into()
            })?,
            Primitive::CAR => Self::operate_top(memory, Memory::car_value)?,
            Primitive::CDR => Self::operate_top(memory, Memory::cdr_value)?,
            Primitive::TYPE => Self::tag(memory, Memory::car_value)?,
            Primitive::TAG => Self::tag(memory, Memory::cdr_value)?,
            Primitive::SET_CAR => Self::set_field(memory, Memory::set_car_value)?,
            Primitive::SET_CDR => Self::set_field(memory, Memory::set_cdr_value)?,
            Primitive::EQUAL => {
                let [x, y] = Self::pop_arguments(memory);
                memory.push(memory.boolean(x == y).into())?;
            }
            Primitive::LESS_THAN => Self::operate_comparison(memory, |x, y| x < y)?,
            Primitive::ADD => Self::operate_binary(memory, Add::add)?,
            Primitive::SUBTRACT => Self::operate_binary(memory, Sub::sub)?,
            Primitive::MULTIPLY => Self::operate_binary(memory, Mul::mul)?,
            Primitive::DIVIDE => Self::operate_binary(memory, Div::div)?,
            Primitive::READ => {
                let byte = self.device.read().map_err(|_| Error::ReadInput)?;

                memory.push(if let Some(byte) = byte {
                    Number::new(byte as i64).into()
                } else {
                    memory.boolean(false).into()
                })?;
            }
            Primitive::WRITE => self.write(memory, Device::write, Error::WriteOutput)?,
            Primitive::WRITE_ERROR => self.write(memory, Device::write_error, Error::WriteError)?,
            Primitive::HALT => return Err(Error::Halt),
            // Optimize type checks.
            Primitive::NULL => Self::check_type(memory, Type::Null)?,
            Primitive::PAIR => Self::check_type(memory, Type::Pair)?,
            Primitive::OPEN_FILE => Self::operate_option(memory, |memory| {
                let [list, output] = Self::pop_arguments(memory);
                let path = Self::decode_path(memory, list)?;
                let output = output != memory.boolean(false).into();

                self.file_system
                    .open(&path, output)
                    .ok()
                    .map(|descriptor| Number::new(descriptor as _).into())
            })?,
            Primitive::CLOSE_FILE => Self::operate_result(memory, |memory| {
                let [descriptor] = Self::pop_number_arguments(memory);

                self.file_system.close(descriptor.to_i64() as _)
            })?,
            Primitive::READ_FILE => Self::operate_option(memory, |memory| {
                let [descriptor] = Self::pop_number_arguments(memory);

                self.file_system
                    .read(descriptor.to_i64() as _)
                    .ok()
                    .map(|byte| Number::new(byte as _).into())
            })?,
            Primitive::WRITE_FILE => Self::operate_result(memory, |memory| {
                let [descriptor, byte] = Self::pop_number_arguments(memory);

                self.file_system
                    .write(descriptor.to_i64() as _, byte.to_i64() as _)
            })?,
            Primitive::DELETE_FILE => Self::operate_option(memory, |memory| {
                let [list] = Self::pop_arguments(memory);
                let path = Self::decode_path(memory, list)?;

                self.file_system
                    .delete(&path)
                    .ok()
                    .map(|_| memory.boolean(true).into())
            })?,
            Primitive::EXISTS_FILE => Self::operate_option(memory, |memory| {
                let [list] = Self::pop_arguments(memory);
                let path = Self::decode_path(memory, list)?;

                self.file_system
                    .exists(&path)
                    .ok()
                    .map(|value| memory.boolean(value).into())
            })?,
            Primitive::COMMAND_LINE => {
                todo!();
            }
            Primitive::ENVIRONMENT_VARIABLES => {
                todo!();
            }
            _ => return Err(Error::Illegal),
        }

        Ok(())
    }
}
