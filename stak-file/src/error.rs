pub enum Error {
    Open,
    Read,
    Write,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Error::Open => write!(f, "cannot open file"),
            Error::Read => write!(f, "cannot read file"),
            Error::Write => write!(f, "cannot write file"),
        }
    }
}
