use super::Read;

pub struct MultiReader<'a> {
    readers: &'a [&'a dyn Read],
}

impl Read for MultiReader<'_> {
    type Error = Error;

    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        self.readers
            .iter_mut()
            .find_map(|r| r.read().transpose())
            .transpose()
    }
}
