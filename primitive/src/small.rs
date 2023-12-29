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

    fn tag<'a>(
        vm: &mut Vm<'a, Self>,
        field: impl Fn(&Vm<'a, Self>, Value) -> Value,
    ) -> Result<(), Error> {
        vm.set_top(
            Number::new(
                field(vm, vm.top())
                    .to_cons()
                    .map(|cons| cons.tag() as _)
                    .unwrap_or(Type::Pair as _),
            )
            .into(),
        );

        Ok(())
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
            Self::attach_tag(field(vm, x), tag.assume_number().to_i64() as u8),
        );
        vm.set_top(tag);

        Ok(())
    }

    fn attach_tag(value: Value, tag: u8) -> Value {
        if let Some(value) = value.to_cons() {
            value.set_tag(tag).into()
        } else {
            value
        }
    }
}

impl<T: Device> PrimitiveSet for SmallPrimitiveSet<T> {
    type Error = Error;

    fn operate(vm: &mut Vm<Self>, primitive: u8) -> Result<(), Error> {
        match primitive {
            Primitive::RIB => {
                let [r#type, car, cdr, tag] = Self::pop_arguments::<4>(vm)?;
                let rib = vm.allocate(
                    Self::attach_tag(car, r#type.assume_number().to_i64() as u8),
                    Self::attach_tag(cdr, tag.assume_number().to_i64() as u8),
                )?;
                vm.set_top(rib.into());
            }
            Primitive::CONS => {
                let [car, cdr] = Self::pop_arguments::<2>(vm)?;
                let cons = vm.allocate(car, Self::attach_tag(cdr, Type::Pair as u8))?;
                vm.set_top(cons.into());
            }
            Primitive::CLOSE => {
                let cons = vm.allocate(
                    vm.car_value(vm.top()),
                    vm.cdr(vm.stack())
                        .assume_cons()
                        .set_tag(Type::Procedure as u8)
                        .into(),
                )?;

                vm.set_top(cons.into());
            }
            Primitive::IS_CONS => {
                vm.set_top(vm.boolean(vm.top().is_cons()).into());
            }
            Primitive::CAR => {
                vm.set_top(vm.car_value(vm.top()));
            }
            Primitive::CDR => {
                vm.set_top(vm.cdr_value(vm.top()));
            }
            Primitive::TAG => Self::tag(vm, Vm::cdr_value)?,
            Primitive::SET_CAR => {
                let [x, y] = Self::pop_arguments::<2>(vm)?;
                vm.set_car_value(x, y);
                vm.set_top(y);
            }
            Primitive::SET_CDR => {
                let [x, y] = Self::pop_arguments::<2>(vm)?;
                // Preserve a tag.
                vm.set_cdr_value(
                    x,
                    y.to_cons()
                        .map(|cons| {
                            cons.set_tag(vm.cdr(x.assume_cons()).assume_cons().tag())
                                .into()
                        })
                        .unwrap_or(y),
                );
                vm.set_top(y);
            }
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
