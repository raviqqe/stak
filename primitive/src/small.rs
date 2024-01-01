mod error;
mod primitive;

use self::{error::Error, primitive::Primitive};
use core::ops::{Add, Div, Mul, Sub};
use device::Device;
use vm::{Number, PrimitiveSet, Type, Value, Vm};

/// A primitive set that covers R7RS small.
pub struct SmallPrimitiveSet<T: Device> {
    device: T,
}

impl<T: Device> SmallPrimitiveSet<T> {
    pub fn new(device: T) -> Self {
        Self { device }
    }

    fn operate_top<'a>(
        vm: &mut Vm<'a, Self>,
        operate: impl Fn(&Vm<'a, Self>, Value) -> Value,
    ) -> Result<(), Error> {
        vm.set_top(operate(vm, vm.top()));

        Ok(())
    }

    fn operate_binary(vm: &mut Vm<Self>, operate: fn(i64, i64) -> i64) -> Result<(), Error> {
        let [x, y] = Self::pop_number_arguments::<2>(vm)?;

        vm.set_top(Number::new(operate(x.to_i64(), y.to_i64())).into());

        Ok(())
    }

    fn operate_comparison(vm: &mut Vm<Self>, operate: fn(i64, i64) -> bool) -> Result<(), Error> {
        let [x, y] = Self::pop_number_arguments::<2>(vm)?;

        vm.set_top(vm.boolean(operate(x.to_i64(), y.to_i64())).into());

        Ok(())
    }

    fn pop_number_arguments<const M: usize>(vm: &mut Vm<Self>) -> Result<[Number; M], Error> {
        let mut numbers = [Default::default(); M];

        for (index, value) in Self::pop_arguments::<M>(vm)?.into_iter().enumerate() {
            numbers[index] = value.assume_number();
        }

        Ok(numbers)
    }

    fn pop_arguments<const M: usize>(vm: &mut Vm<Self>) -> Result<[Value; M], Error> {
        let mut values = [Default::default(); M];

        for index in 0..M - 1 {
            values[M - 1 - index] = vm.pop()?;
        }

        values[0] = vm.top();

        Ok(values)
    }

    fn set_field<'a>(
        vm: &mut Vm<'a, Self>,
        field: fn(&Vm<'a, Self>, Value) -> Value,
        set_field: fn(&mut Vm<'a, Self>, Value, Value),
    ) -> Result<(), Error> {
        let [x, y] = Self::pop_arguments::<2>(vm)?;
        // Preserve a tag.
        set_field(
            vm,
            x,
            if let (Some(x), Some(y)) = (field(vm, x).to_cons(), y.to_cons()) {
                y.set_tag(x.tag()).into()
            } else {
                y
            },
        );
        vm.set_top(y);

        Ok(())
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

    fn set_tag<'a>(
        vm: &mut Vm<'a, Self>,
        field: fn(&Vm<'a, Self>, Value) -> Value,
        set_field: fn(&mut Vm<'a, Self>, Value, Value),
    ) -> Result<(), Error> {
        let [x, tag] = Self::pop_arguments::<2>(vm)?;
        set_field(
            vm,
            x,
            field(vm, x).set_tag(tag.assume_number().to_i64() as u8),
        );
        vm.set_top(tag);

        Ok(())
    }
}

impl<T: Device> PrimitiveSet for SmallPrimitiveSet<T> {
    type Error = Error;

    fn operate(vm: &mut Vm<Self>, primitive: u8) -> Result<(), Error> {
        match primitive {
            Primitive::RIB => {
                let [r#type, car, cdr, tag] = Self::pop_arguments::<4>(vm)?;
                let rib = vm.allocate(
                    car.set_tag(r#type.assume_number().to_i64() as u8),
                    cdr.set_tag(tag.assume_number().to_i64() as u8),
                )?;
                vm.set_top(rib.into());
            }
            Primitive::CONS => {
                let [car, cdr] = Self::pop_arguments::<2>(vm)?;
                let cons = vm.allocate(car.set_tag(Type::Pair as u8), cdr)?;
                vm.set_top(cons.into());
            }
            Primitive::CLOSE => {
                let cons = vm.allocate(
                    vm.cdr(vm.stack())
                        .assume_cons()
                        .set_tag(Type::Procedure as u8)
                        .into(),
                    vm.cdr_value(vm.top()),
                )?;

                vm.set_top(cons.into());
            }
            Primitive::IS_CONS => {
                Self::operate_top(vm, |vm, value| vm.boolean(value.is_cons()).into())?
            }
            Primitive::CAR => Self::operate_top(vm, Vm::car_value)?,
            Primitive::CDR => Self::operate_top(vm, Vm::cdr_value)?,
            Primitive::TAG => Self::tag(vm, Vm::cdr_value)?,
            Primitive::SET_CAR => Self::set_field(vm, Vm::car_value, Vm::set_car_value)?,
            Primitive::SET_CDR => Self::set_field(vm, Vm::cdr_value, Vm::set_cdr_value)?,
            Primitive::SET_TAG => Self::set_tag(vm, Vm::cdr_value, Vm::set_cdr_value)?,
            Primitive::EQUAL => {
                let [x, y] = Self::pop_arguments::<2>(vm)?;
                vm.set_top(vm.boolean(x == y).into());
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
            Primitive::WRITE => {
                let byte = vm.top().assume_number().to_i64() as u8;

                vm.primitive_set_mut()
                    .device
                    .write(byte)
                    .map_err(|_| Error::WriteOutput)?
            }
            Primitive::WRITE_ERROR => {
                let byte = vm.top().assume_number().to_i64() as u8;

                vm.primitive_set_mut()
                    .device
                    .write_error(byte)
                    .map_err(|_| Error::WriteError)?
            }
            Primitive::HALT => return Err(Error::Halt),
            Primitive::TYPE => Self::tag(vm, Vm::car_value)?,
            Primitive::SET_TYPE => Self::set_tag(vm, Vm::car_value, Vm::set_car_value)?,
            _ => return Err(Error::Illegal),
        }

        Ok(())
    }
}
