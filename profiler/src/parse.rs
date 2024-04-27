use crate::{error::Error, record::ProcedureRecord};
use std::io::BufRead;

/// Parses records.
pub fn parse_records(reader: impl BufRead) -> impl Iterator<Item = Result<ProcedureRecord, Error>> {
    reader
        .lines()
        .map(|line| -> Result<ProcedureRecord, Error> { line?.parse() })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::record_type::ProcedureRecordType;
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
                Ok(ProcedureRecord::new(
                    ProcedureRecordType::Call,
                    vec![Some("baz".into()), Some("bar".into()), Some("foo".into())],
                    0
                )),
                Ok(ProcedureRecord::new(
                    ProcedureRecordType::Return,
                    vec![Some("baz".into()), Some("bar".into()), Some("foo".into())],
                    42
                ))
            ]
        );
    }
}
