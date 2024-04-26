//! Profiling for Stak Scheme.

mod error;
mod record;
mod record_type;

pub use error::Error;
pub use record::Record;
pub use record_type::RecordType;

pub fn parse_records<'a>(source: &'a str) -> impl Iterator<Item = Result<Record<'a>, Error>> + '_ {
    source.lines().map(|line| -> Result<Record, Error> {
        let mut iterator = line.split("\t");

        Ok(Record::new(
            match iterator.next().ok_or(Error::MissingRecordType)? {
                "call" => RecordType::Call,
                "return" => RecordType::Return,
                "return_call" => RecordType::ReturnCall,
                _ => return Err(Error::UnknownRecordType),
            },
            iterator
                .next()
                .ok_or(Error::MissingStack)?
                .split(";")
                .map(ToOwned::to_owned)
                .collect(),
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
                &indoc!(
                    "
                    call\tfoo;bar;baz\t0
                    return\tfoo;bar;baz\t42
                    "
                )
                .trim()
            )
            .collect::<Vec<_>>(),
            vec![
                Ok(Record::new(RecordType::Call, vec!["foo", "bar", "baz"], 0)),
                Ok(Record::new(
                    RecordType::Return,
                    vec!["foo", "bar", "baz"],
                    0
                ))
            ]
        );
    }
}
