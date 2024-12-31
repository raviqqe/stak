mod primitive;

pub use self::primitive::Primitive;
use crate::FileSystem;
use stak_vm::{Error, Memory, Number, PrimitiveSet, Value};

/// A primitive set for a file system.
pub struct FilePrimitiveSet<T: FileSystem> {
    file_system: T,
}

impl<T: FileSystem> FilePrimitiveSet<T> {
    /// Creates a primitive set.
    pub const fn new(file_system: T) -> Self {
        Self { file_system }
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
            Primitive::OPEN_FILE => memory.operate_option(|memory| {
                let [list, output] = memory.pop_many();
                let path = T::decode_path(memory, list).ok()?;
                let output = output != memory.boolean(false).into();

                self.file_system
                    .open(path.as_ref(), output)
                    .ok()
                    .map(|descriptor| Number::new(descriptor as _).into())
            })?,
            Primitive::CLOSE_FILE => Self::operate_result(memory, |memory| {
                let [descriptor] = memory.pop_numbers();

                self.file_system.close(descriptor.to_i64() as _)
            })?,
            Primitive::READ_FILE => memory.operate_option(|memory| {
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
            Primitive::DELETE_FILE => memory.operate_option(|memory| {
                let [list] = memory.pop_many();
                let path = T::decode_path(memory, list).ok()?;

                self.file_system
                    .delete(path.as_ref())
                    .ok()
                    .map(|_| memory.boolean(true).into())
            })?,
            Primitive::EXISTS_FILE => memory.operate_option(|memory| {
                let [list] = memory.pop_many();
                let path = T::decode_path(memory, list).ok()?;

                self.file_system
                    .exists(path.as_ref())
                    .ok()
                    .map(|value| memory.boolean(value).into())
            })?,
            _ => return Err(Error::IllegalPrimitive),
        }

        Ok(())
    }
}
