use crate::{error::Error, record::Record, COLUMN_SEPARATOR, FRAME_SEPARATOR};
use std::io::BufRead;

/// Parses records.
pub fn parse_records(reader: impl BufRead) -> impl Iterator<Item = Result<Record, Error>> {
    reader.lines().map(|line| -> Result<Record, Error> {
        let line = line?;
        let mut iterator = line.split(COLUMN_SEPARATOR);

        Ok(Record::new(
            iterator.next().ok_or(Error::MissingRecordType)?.parse()?,
            {
                let mut stack = iterator
                    .next()
                    .ok_or(Error::MissingStack)?
                    .split(FRAME_SEPARATOR)
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
    use crate::record_type::RecordType;
    use indoc::indoc;
    use pretty_assertions::assert_eq;
    use std::io::BufReader;

    #[test]
    fn parse_record() {
        assert_eq!(
            parse_records(BufReader::new(
                indoc!(
                    "
                    call\tfoo;bar;baz\t0
                    return\tfoo;bar;baz\t42
                    "
                )
                .trim()
                .as_bytes()
            ))
            .collect::<Vec<_>>(),
            vec![
                Ok(Record::new(
                    RecordType::Call,
                    vec!["baz".into(), "bar".into(), "foo".into()],
                    0
                )),
                Ok(Record::new(
                    RecordType::Return,
                    vec!["baz".into(), "bar".into(), "foo".into()],
                    42
                ))
            ]
        );
    }
}
