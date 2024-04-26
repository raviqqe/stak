use crate::{error::Error, record::Record, record_type::RecordType};
use std::io::BufRead;

/// Parses records.
pub fn parse_records(reader: impl BufRead) -> impl Iterator<Item = Result<Record, Error>> {
    reader.lines().map(|line| -> Result<Record, Error> {
        let line = line?;
        let mut iterator = line.split('\t');

        Ok(Record::new(
            match iterator.next().ok_or(Error::MissingRecordType)? {
                "call" => RecordType::Call,
                "return" => RecordType::Return,
                "return_call" => RecordType::ReturnCall,
                _ => return Err(Error::UnknownRecordType),
            },
            {
                let mut stack = iterator
                    .next()
                    .ok_or(Error::MissingStack)?
                    .split(';')
                    .map(ToOwned::to_owned)
                    .collect::<Vec<_>>();
                stack.reverse();
                stack
            },
            iterator.next().ok_or(Error::MissingTime)?.parse()?,
        ))
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    #[test]
    fn parse_record() {
        assert_eq!(
            parse_records(
                indoc!(
                    "
                    call\tfoo;bar;baz\t0
                    return\tfoo;bar;baz\t42
                    "
                )
                .trim()
            )
            .collect::<Vec<_>>(),
            vec![
                Ok(Record::new(RecordType::Call, vec!["baz", "bar", "foo"], 0)),
                Ok(Record::new(
                    RecordType::Return,
                    vec!["baz", "bar", "foo"],
                    42
                ))
            ]
        );
    }
}
