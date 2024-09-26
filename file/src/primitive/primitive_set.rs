pub use super::error::PrimitiveError;
use super::Primitive;
use crate::FileSystem;
use heapless::Vec;
use stak_vm::{Memory, Number, PrimitiveSet, Value};

const PATH_SIZE: usize = 64;

/// A primitive set for a file system.
pub struct FilePrimitiveSet<T: FileSystem> {
    file_system: T,
}

impl<T: FileSystem> FilePrimitiveSet<T> {
    /// Creates a primitive set.
    pub const fn new(file_system: T) -> Self {
        Self { file_system }
    }

    /// Returns a reference to a file system.
    pub const fn file_system(&self) -> &T {
        &self.file_system
    }

    /// Returns a mutable reference to a file system.
    pub fn file_system_mut(&mut self) -> &mut T {
        &mut self.file_system
    }

    fn operate_option<'a>(
        memory: &mut Memory<'a>,
        operate: impl Fn(&mut Memory<'a>) -> Option<Value>,
    ) -> Result<(), PrimitiveError> {
        let value = operate(memory).unwrap_or_else(|| memory.boolean(false).into());
        memory.push(value)?;
        Ok(())
    }

    fn operate_result<'a, E>(
        memory: &mut Memory<'a>,
        operate: impl Fn(&mut Memory<'a>) -> Result<(), E>,
    ) -> Result<(), PrimitiveError> {
        let result = operate(memory);
        memory.push(memory.boolean(result.is_ok()).into())?;
        Ok(())
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

impl<T: FileSystem> PrimitiveSet for FilePrimitiveSet<T> {
    type Error = PrimitiveError;

    fn operate(&mut self, memory: &mut Memory, primitive: u8) -> Result<(), Self::Error> {
        match primitive {
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
            _ => return Err(stak_vm::Error::IllegalPrimitive.into()),
        }

        Ok(())
    }
}
