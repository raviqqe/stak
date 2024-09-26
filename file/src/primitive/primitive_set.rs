use super::Primitive;
use crate::FileSystem;
use heapless::Vec;
use stak_vm::Error;
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
    type Error = Error;

    fn operate(&mut self, memory: &mut Memory, primitive: u8) -> Result<(), Self::Error> {
        match primitive {
            Primitive::OPEN_FILE => Self::operate_option(memory, |memory| {
                let [list, output] = memory.pop_many();
                let path = Self::decode_path(memory, list)?;
                let output = output != memory.boolean(false).into();

                self.file_system
                    .open(&path, output)
                    .ok()
                    .map(|descriptor| Number::new(descriptor as _).into())
            })?,
            Primitive::CLOSE_FILE => Self::operate_result(memory, |memory| {
                let [descriptor] = memory.pop_numbers();

                self.file_system.close(descriptor.to_i64() as _)
            })?,
            Primitive::READ_FILE => Self::operate_option(memory, |memory| {
                let [descriptor] = memory.pop_numbers();

                self.file_system
                    .read(descriptor.to_i64() as _)
                    .ok()
                    .map(|byte| Number::new(byte as _).into())
            })?,
            Primitive::WRITE_FILE => Self::operate_result(memory, |memory| {
                let [descriptor, byte] = memory.pop_numbers();

                self.file_system
                    .write(descriptor.to_i64() as _, byte.to_i64() as _)
            })?,
            Primitive::DELETE_FILE => Self::operate_option(memory, |memory| {
                let [list] = memory.pop_many();
                let path = Self::decode_path(memory, list)?;

                self.file_system
                    .delete(&path)
                    .ok()
                    .map(|_| memory.boolean(true).into())
            })?,
            Primitive::EXISTS_FILE => Self::operate_option(memory, |memory| {
                let [list] = memory.pop_many();
                let path = Self::decode_path(memory, list)?;

                self.file_system
                    .exists(&path)
                    .ok()
                    .map(|value| memory.boolean(value).into())
            })?,
            _ => return Err(Error::IllegalPrimitive),
        }

        Ok(())
    }
}
