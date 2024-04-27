use crate::{error::Error, procedure_record::ProcedureRecord};
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
    use crate::procedure_operation::ProcedureOperation;
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
                    ProcedureOperation::Call,
                    vec![Some("baz".into()), Some("bar".into()), Some("foo".into())],
                    0
                )),
                Ok(ProcedureRecord::new(
                    ProcedureOperation::Return,
                    vec![Some("baz".into()), Some("bar".into()), Some("foo".into())],
                    42
                ))
            ]
        );
    }
}
