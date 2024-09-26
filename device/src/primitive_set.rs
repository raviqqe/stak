use crate::Device;
use stak_vm::{Cons, Memory, Number, NumberRepresentation, PrimitiveSet, Tag, Type, Value};

enum Error {}

/// A primitive set for devices.
pub struct DevicePrimitiveSet<const READ: u8, const WRITE: u8, const WRITE_ERROR: u8, D: Device> {
    device: D,
}

impl<D: Device> DevicePrimitiveSet<D> {
    /// Creates a primitive set.
    pub fn new(device: D) -> Self {
        Self { device }
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

    fn operate_unary(memory: &mut Memory, operate: fn(Number) -> Number) -> Result<(), Error> {
        let [x] = Self::pop_number_arguments(memory);

        memory.push(operate(x).into())?;

        Ok(())
    }

    fn operate_binary(
        memory: &mut Memory,
        operate: fn(Number, Number) -> Number,
    ) -> Result<(), Error> {
        let [x, y] = Self::pop_number_arguments(memory);

        memory.push(operate(x, y).into())?;

        Ok(())
    }

    fn operate_comparison(
        memory: &mut Memory,
        operate: fn(NumberRepresentation, NumberRepresentation) -> bool,
    ) -> Result<(), Error> {
        let [x, y] = Self::pop_number_arguments(memory);

        memory.push(
            memory
                .boolean(operate(x.to_representation(), y.to_representation()))
                .into(),
        )?;
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
            field(vm, value)
                .to_cons()
                .map(|cons| Number::new(cons.tag() as _))
                .unwrap_or(Number::from_i64(Type::default() as _))
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

    fn build_string(memory: &mut Memory, string: &str) -> Result<Cons, Error> {
        let mut list = memory.null();

        for character in string.chars().rev() {
            list = memory.cons(Number::from_i64(character as _).into(), list)?;
        }

        Ok(list)
    }
}

impl<const READ: u8, const WRITE: u8, const WRITE_ERROR: u8, D: Device> PrimitiveSet
    for DevicePrimitiveSet<READ, WRITE, WRITE_ERROR, D>
{
    type Error = Error;

    fn operate(&mut self, memory: &mut Memory, primitive: u8) -> Result<(), Self::Error> {
        match primitive {
            READ => {
                let byte = self.device.read().map_err(|_| Error::ReadInput)?;

                memory.push(if let Some(byte) = byte {
                    Number::from_i64(byte as _).into()
                } else {
                    memory.boolean(false).into()
                })?;
            }
            WRITE => self.write(memory, Device::write, Error::WriteOutput)?,
            WRITE_ERROR => self.write(memory, Device::write_error, Error::WriteError)?,
            _ => return Err(Error::Illegal),
        }

        Ok(())
    }
}
