use super::{error::Error, Read};

#[derive(Clone, Copy, Debug, Default)]
pub struct Stdin;

impl Read for Stdin {
    type Error = Error;

    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        let mut bytes = [0];

        Ok(
            if unsafe { libc::read(libc::STDIN_FILENO, &mut bytes as *mut _ as _, 1) } == 1 {
                Some(bytes[0])
            } else {
                None
            },
        )
    }
}
