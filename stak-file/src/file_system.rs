pub trait FileSystem<E> {
    fn open(&self, path: &[u8]) -> Result<usize, E>;
    fn read(&self, descriptor: usize) -> Result<u8, E>;
    fn write(&self, descriptor: usize, byte: u8) -> Result<(), E>;
}
