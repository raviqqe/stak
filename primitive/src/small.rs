mod error;
mod primitive;

pub use self::error::Error;
use self::primitive::Primitive;
use core::ops::{Add, Div, Mul, Sub};
use stak_device::Device;
use stak_vm::{Number, PrimitiveSet, Tag, Type, Value, Vm};

/// A primitive set that covers R7RS small.
pub struct SmallPrimitiveSet<T: Device> {
    device: T,
}

impl<T: Device> SmallPrimitiveSet<T> {
    /// Creates a primitive set.
    pub fn new(device: T) -> Self {
        Self { device }
    }

    /// Returns a reference to a device.
    pub fn device(&self) -> &T {
        &self.device
    }

    /// Returns a mutable reference to a device.
    pub fn device_mut(&mut self) -> &mut T {
        &mut self.device
    }

    fn operate_top<'a>(vm: &mut Vm<'a, Self>, operate: impl Fn(&Vm<'a, Self>, Value) -> Value) {
        vm.set_top(operate(vm, vm.top()));
    }

    fn operate_binary(vm: &mut Vm<Self>, operate: fn(i64, i64) -> i64) {
        let [x, y] = Self::pop_number_arguments(vm);

        vm.set_top(Number::new(operate(x.to_i64(), y.to_i64())).into());
    }

    fn operate_comparison(vm: &mut Vm<Self>, operate: fn(i64, i64) -> bool) {
        let [x, y] = Self::pop_number_arguments(vm);

        vm.set_top(vm.boolean(operate(x.to_i64(), y.to_i64())).into());
    }

    fn rib(vm: &mut Vm<Self>, r#type: Tag, car: Value, cdr: Value) -> Result<(), Error> {
        let rib = vm.allocate(car.set_tag(r#type), cdr)?;
        vm.set_top(rib.into());

        Ok(())
    }

    fn set_field<'a>(vm: &mut Vm<'a, Self>, set_field: fn(&mut Vm<'a, Self>, Value, Value)) {
        let [x, y] = Self::pop_arguments(vm);

        set_field(vm, x, y);
        vm.set_top(y);
    }

    fn tag<'a>(vm: &mut Vm<'a, Self>, field: impl Fn(&Vm<'a, Self>, Value) -> Value) {
        Self::operate_top(vm, |vm, value| {
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
        vm: &mut Vm<Self>,
        write: fn(&mut T, u8) -> Result<(), <T as Device>::Error>,
        error: Error,
    ) -> Result<(), Error> {
        let byte = vm.top().assume_number().to_i64() as u8;

        write(&mut vm.primitive_set_mut().device, byte).map_err(|_| error)
    }

    fn pop_number_arguments<const M: usize>(vm: &mut Vm<Self>) -> [Number; M] {
        let mut numbers = [Default::default(); M];

        for (index, value) in Self::pop_arguments::<M>(vm).into_iter().enumerate() {
            numbers[index] = value.assume_number();
        }

        numbers
    }

    fn pop_arguments<const M: usize>(vm: &mut Vm<Self>) -> [Value; M] {
        let mut values = [Default::default(); M];

        for index in 0..M - 1 {
            values[M - 1 - index] = vm.pop();
        }

        values[0] = vm.top();

        values
    }
}

impl<T: Device> PrimitiveSet for SmallPrimitiveSet<T> {
    type Error = Error;

    fn operate(vm: &mut Vm<Self>, primitive: u8) -> Result<(), Error> {
        match primitive {
            Primitive::RIB => {
                let [r#type, car, cdr, tag] = Self::pop_arguments(vm);

                Self::rib(
                    vm,
                    r#type.assume_number().to_i64() as Tag,
                    car,
                    cdr.set_tag(tag.assume_number().to_i64() as Tag),
                )?;
            }
            Primitive::CONS => {
                let [car, cdr] = Self::pop_arguments(vm);

                Self::rib(vm, Type::Pair as Tag, car, cdr)?;
            }
            Primitive::CLOSE => {
                Self::rib(
                    vm,
                    Type::Procedure as Tag,
                    vm.cdr(vm.stack()),
                    vm.cdr_value(vm.top()),
                )?;
            }
            Primitive::IS_RIB => {
                Self::operate_top(vm, |vm, value| vm.boolean(value.is_cons()).into())
            }
            Primitive::CAR => Self::operate_top(vm, Vm::car_value),
            Primitive::CDR => Self::operate_top(vm, Vm::cdr_value),
            Primitive::TYPE => Self::tag(vm, Vm::car_value),
            Primitive::TAG => Self::tag(vm, Vm::cdr_value),
            Primitive::SET_CAR => Self::set_field(vm, Vm::set_car_value),
            Primitive::SET_CDR => Self::set_field(vm, Vm::set_cdr_value),
            Primitive::EQUAL => {
                let [x, y] = Self::pop_arguments(vm);
                vm.set_top(vm.boolean(x == y).into());
            }
            Primitive::LESS_THAN => Self::operate_comparison(vm, |x, y| x < y),
            Primitive::ADD => Self::operate_binary(vm, Add::add),
            Primitive::SUBTRACT => Self::operate_binary(vm, Sub::sub),
            Primitive::MULTIPLY => Self::operate_binary(vm, Mul::mul),
            Primitive::DIVIDE => Self::operate_binary(vm, Div::div),
            Primitive::READ => {
                let byte = vm
                    .primitive_set_mut()
                    .device
                    .read()
                    .map_err(|_| Error::ReadInput)?;

                vm.push(if let Some(byte) = byte {
                    Number::new(byte as i64).into()
                } else {
                    vm.boolean(false).into()
                })?;
            }
            Primitive::WRITE => Self::write(vm, Device::write, Error::WriteOutput)?,
            Primitive::WRITE_ERROR => Self::write(vm, Device::write_error, Error::WriteError)?,
            Primitive::HALT => return Err(Error::Halt),
            Primitive::NULL => Self::operate_top(vm, |vm, value| Vm::car_value(vm, value)),
            Primitive::PAIR => Self::operate_top(vm, |vm, value| Vm::car_value(vm, value)),
            _ => return Err(Error::Illegal),
        }

        Ok(())
    }
}
