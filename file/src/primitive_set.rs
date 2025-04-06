mod error;
mod primitive;

pub use self::primitive::Primitive;
use crate::{FileError, FileSystem};
pub use error::PrimitiveError;
use stak_vm::{Error, Memory, Number, PrimitiveSet};

/// A primitive set for a file system.
pub struct FilePrimitiveSet<T: FileSystem> {
    file_system: T,
}

impl<T: FileSystem> FilePrimitiveSet<T> {
    /// Creates a primitive set.
    pub const fn new(file_system: T) -> Self {
        Self { file_system }
    }
}

impl<T: FileSystem> PrimitiveSet for FilePrimitiveSet<T> {
    type Error = PrimitiveError;

    fn operate(&mut self, memory: &mut Memory, primitive: usize) -> Result<(), Self::Error> {
        match primitive {
            Primitive::OPEN_FILE => {
                let [list, output] = memory.pop_many();
                let path = T::decode_path(memory, list).map_err(|_| FileError::PathDecode)?;
                let output = output != memory.boolean(false).into();

                memory.push(
                    Number::new(
                        self.file_system
                            .open(path.as_ref(), output)
                            .map_err(|_| FileError::Open)? as _,
                    )
                    .into(),
                )?;
            }
            Primitive::CLOSE_FILE => {
                let [descriptor] = memory.pop_numbers();

                self.file_system
                    .close(descriptor.to_i64() as _)
                    .map_err(|_| FileError::Close)?;

                memory.push(memory.boolean(false).into())?;
            }
            Primitive::READ_FILE => {
                let [descriptor] = memory.pop_numbers();

                memory.push(self.file_system.read(descriptor.to_i64() as _).map_or_else(
                    |_| memory.boolean(false).into(),
                    |byte| Number::new(byte as _).into(),
                ))?;
            }
            Primitive::WRITE_FILE => {
                let [descriptor, byte] = memory.pop_numbers();

                self.file_system
                    .write(descriptor.to_i64() as _, byte.to_i64() as _)
                    .map_err(|_| FileError::Write)?;

                memory.push(memory.boolean(false).into())?;
            }
            Primitive::DELETE_FILE => {
                let [list] = memory.pop_many();
                let path = T::decode_path(memory, list).map_err(|_| FileError::PathDecode)?;
                self.file_system
                    .delete(path.as_ref())
                    .map_err(|_| FileError::Delete)?;

                memory.push(memory.boolean(false).into())?;
            }
            Primitive::EXISTS_FILE => {
                let [list] = memory.pop_many();
                let path = T::decode_path(memory, list).map_err(|_| FileError::PathDecode)?;

                memory.push(
                    memory
                        .boolean(
                            self.file_system
                                .exists(path.as_ref())
                                .map_err(|_| FileError::Exists)?,
                        )
                        .into(),
                )?;
            }
            _ => return Err(Error::IllegalPrimitive.into()),
        }

        Ok(())
    }
}
