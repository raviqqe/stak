mod error;
mod primitive;

pub use self::error::Error;
use self::primitive::Primitive;
use core::ops::{Add, Div, Mul, Sub};
use heapless::Vec;
use stak_device::Device;
use stak_file::FileSystem;
use stak_vm::{Number, PrimitiveSet, Tag, Type, Value, Vm};

const PATH_SIZE: usize = 64;

/// A primitive set that covers R7RS small.
pub struct SmallPrimitiveSet<D: Device, F: FileSystem> {
    device: D,
    file_system: F,
}

impl<D: Device, F: FileSystem> SmallPrimitiveSet<D, F> {
    /// Creates a primitive set.
    pub fn new(device: D, file_system: F) -> Self {
        Self {
            device,
            file_system,
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
        vm: &mut Vm<'a, Self>,
        operate: impl Fn(&Vm<'a, Self>, Value) -> Value,
    ) -> Result<(), Error> {
        let x = vm.pop();
        vm.push(operate(vm, x))
    }

    fn operate_binary(vm: &mut Vm<Self>, operate: fn(i64, i64) -> i64) -> Result<(), Error> {
        let [x, y] = Self::pop_number_arguments(vm);

        vm.push(Number::new(operate(x.to_i64(), y.to_i64())).into())
    }

    fn operate_comparison(vm: &mut Vm<Self>, operate: fn(i64, i64) -> bool) -> Result<(), Error> {
        let [x, y] = Self::pop_number_arguments(vm);

        vm.push(vm.boolean(operate(x.to_i64(), y.to_i64())).into())
    }

    fn operate_option<'a>(
        vm: &mut Vm<'a, Self>,
        operate: impl Fn(&mut Vm<'a, Self>) -> Option<Value>,
    ) -> Result<(), Error> {
        let value = operate(vm).unwrap_or_else(|| vm.boolean(false).into());

        vm.push(value)
    }

    fn operate_result<'a, E>(
        vm: &mut Vm<'a, Self>,
        operate: impl Fn(&mut Vm<'a, Self>) -> Result<(), E>,
    ) -> Result<(), Error> {
        let result = operate(vm);
        vm.push(vm.boolean(result.is_err()).into())
    }

    fn rib(vm: &mut Vm<Self>, r#type: Tag, car: Value, cdr: Value) -> Result<(), Error> {
        let rib = vm.allocate(car.set_tag(r#type), cdr)?;
        vm.push(rib.into())
    }

    fn set_field<'a>(
        vm: &mut Vm<'a, Self>,
        set_field: fn(&mut Vm<'a, Self>, Value, Value),
    ) -> Result<(), Error> {
        let [x, y] = Self::pop_arguments(vm);

        set_field(vm, x, y);
        vm.push(y)
    }

    fn tag<'a>(
        vm: &mut Vm<'a, Self>,
        field: impl Fn(&Vm<'a, Self>, Value) -> Value,
    ) -> Result<(), Error> {
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
        write: fn(&mut D, u8) -> Result<(), <D as Device>::Error>,
        error: Error,
    ) -> Result<(), Error> {
        let byte = vm.top().assume_number().to_i64() as u8;

        write(&mut vm.primitive_set_mut().device, byte).map_err(|_| error)
    }

    fn check_type(vm: &mut Vm<Self>, r#type: Type) -> Result<(), Error> {
        Self::operate_top(vm, |vm, value| {
            vm.boolean(
                value
                    .to_cons()
                    .map(|cons| vm.car(cons).tag() == r#type as Tag)
                    .unwrap_or_default(),
            )
            .into()
        })
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

        values[0] = vm.pop();

        values
    }
}

impl<D: Device, F: FileSystem> PrimitiveSet for SmallPrimitiveSet<D, F> {
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
            // Optimize a cons.
            Primitive::CONS => {
                let [car, cdr] = Self::pop_arguments(vm);

                Self::rib(vm, Type::Pair as Tag, car, cdr)?;
            }
            Primitive::CLOSE => {
                let closure = vm.pop();

                Self::rib(
                    vm,
                    Type::Procedure as Tag,
                    vm.stack().into(),
                    vm.cdr_value(closure),
                )?;
            }
            Primitive::IS_RIB => {
                Self::operate_top(vm, |vm, value| vm.boolean(value.is_cons()).into())?
            }
            Primitive::CAR => Self::operate_top(vm, Vm::car_value)?,
            Primitive::CDR => Self::operate_top(vm, Vm::cdr_value)?,
            Primitive::TYPE => Self::tag(vm, Vm::car_value)?,
            Primitive::TAG => Self::tag(vm, Vm::cdr_value)?,
            Primitive::SET_CAR => Self::set_field(vm, Vm::set_car_value)?,
            Primitive::SET_CDR => Self::set_field(vm, Vm::set_cdr_value)?,
            Primitive::EQUAL => {
                let [x, y] = Self::pop_arguments(vm);
                vm.push(vm.boolean(x == y).into())?;
            }
            Primitive::LESS_THAN => Self::operate_comparison(vm, |x, y| x < y)?,
            Primitive::ADD => Self::operate_binary(vm, Add::add)?,
            Primitive::SUBTRACT => Self::operate_binary(vm, Sub::sub)?,
            Primitive::MULTIPLY => Self::operate_binary(vm, Mul::mul)?,
            Primitive::DIVIDE => Self::operate_binary(vm, Div::div)?,
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
            // Optimize type checks.
            Primitive::NULL => Self::check_type(vm, Type::Null)?,
            Primitive::PAIR => Self::check_type(vm, Type::Pair)?,
            Primitive::OPEN_FILE => Self::operate_option(vm, |vm| {
                let [list, output] = Self::pop_arguments(vm);
                let mut path = Vec::<_, PATH_SIZE>::new();

                while list.assume_cons() != vm.null() {
                    path.push(vm.car_value(list).assume_number().to_i64() as u8)
                        .ok()?;
                }

                let output = output != vm.boolean(false).into();

                vm.primitive_set_mut()
                    .file_system
                    .open(&path, output)
                    .map(|descriptor| Number::new(descriptor as _).into())
                    .ok()
            })?,
            Primitive::CLOSE_FILE => Self::operate_result(vm, |vm| {
                let [descriptor] = Self::pop_number_arguments(vm);

                vm.primitive_set_mut()
                    .file_system
                    .close(descriptor.to_i64() as _)
            })?,
            Primitive::READ_FILE => Self::operate_option(vm, |vm| {
                let [descriptor] = Self::pop_number_arguments(vm);

                vm.primitive_set_mut()
                    .file_system
                    .read(descriptor.to_i64() as _)
                    .ok()
                    .map(|byte| Number::new(byte as _).into())
            })?,
            Primitive::WRITE_FILE => Self::operate_result(vm, |vm| {
                let [descriptor, byte] = Self::pop_number_arguments(vm);

                vm.primitive_set_mut()
                    .file_system
                    .write(descriptor.to_i64() as _, byte.to_i64() as _)
            })?,
            _ => return Err(Error::Illegal),
        }

        Ok(())
    }
}
