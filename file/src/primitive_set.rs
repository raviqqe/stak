mod primitive;

pub use self::primitive::Primitive;
use crate::FileSystem;
use stak_vm::{Error, Memory, Number, PrimitiveSet, Value};

const PATH_SIZE: usize = 128;

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
        mut operate: impl FnMut(&mut Memory<'a>) -> Option<Value>,
    ) -> Result<(), Error> {
        let value = operate(memory).unwrap_or_else(|| memory.boolean(false).into());
        memory.push(value)?;
        Ok(())
    }

    fn operate_result<'a, E>(
        memory: &mut Memory<'a>,
        mut operate: impl FnMut(&mut Memory<'a>) -> Result<(), E>,
    ) -> Result<(), Error> {
        let result = operate(memory);
        memory.push(memory.boolean(result.is_ok()).into())?;
        Ok(())
    }
}

impl<T: FileSystem> PrimitiveSet for FilePrimitiveSet<T> {
    type Error = Error;

    fn operate(&mut self, memory: &mut Memory, primitive: usize) -> Result<(), Self::Error> {
        match primitive {
            Primitive::OPEN_FILE => Self::operate_option(memory, |memory| {
                let [list, output] = memory.pop_many();
                let path = self.file_system.decode_path(memory, list).ok()?;
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
                let path = self.file_system.decode_path(memory, list).ok()?;

                self.file_system
                    .delete(&path)
                    .ok()
                    .map(|_| memory.boolean(true).into())
            })?,
            Primitive::EXISTS_FILE => Self::operate_option(memory, |memory| {
                let [list] = memory.pop_many();
                let path = self.file_system.decode_path(memory, list).ok()?;

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
