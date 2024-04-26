use core::num::ParseIntError;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Record {
    r#type: RecordType,
    stack: Vec<String>,
    time: u128,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RecordType {
    Call,
    Return,
    ReturnCall,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Error {
    MissingRecordType,
    MissingStack,
    MissingTime,
    ParseInt(ParseIntError),
    UnknownRecordType,
}

impl From<ParseIntError> for Error {
    fn from(error: ParseIntError) -> Self {
        Self::ParseInt(error)
    }
}

pub fn parse_records(source: &str) -> impl Iterator<Item = Result<Record, Error>> + '_ {
    source.lines().map(|line| -> Result<Record, Error> {
        let mut iterator = line.split("\t");

        Ok(Record {
            r#type: match iterator.next().ok_or(Error::MissingRecordType)? {
                "call" => RecordType::Call,
                "return" => RecordType::Return,
                "return_call" => RecordType::ReturnCall,
                _ => return Err(Error::UnknownRecordType),
            },
            stack: iterator
                .next()
                .ok_or(Error::MissingStack)?
                .split(";")
                .map(ToOwned::to_owned)
                .collect(),
            time: iterator.next().ok_or(Error::MissingTime)?.parse()?,
        })
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn parse_record() {
        assert_eq!(
            parse_records(
                &indoc!(
                    "
                    call\tfoo;bar;baz\t0
                    return\tfoo;bar;baz\t42
                    "
                )
                .trim()
            )
            .collect::<Vec<_>>(),
            vec![]
        );
    }
}
