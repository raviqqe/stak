use crate::Record;
use core::str::FromStr;
use std::io::{self, BufRead};

/// Reads profile records.
pub fn read_records<R: Record>(
    reader: impl BufRead,
) -> impl Iterator<Item = Result<R, <R as FromStr>::Err>>
where
    <R as FromStr>::Err: From<io::Error>,
{
    reader
        .lines()
        .map(|line| Ok(line?.parse().ok()))
        .filter_map(Result::transpose)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ProcedureOperation, ProcedureRecord, Stack};
    use indoc::indoc;
    use pretty_assertions::assert_eq;
    use std::io::BufReader;

    #[test]
    fn read() {
        // spell-checker: disable
        assert_eq!(
            read_records::<ProcedureRecord>(BufReader::new(
                indoc!(
                    "
                    call\tbaz\t0
                    return\tfoo;bar\t42
                    return_call\t;;\t2045
                    "
                )
                .as_bytes()
            ))
            .collect::<Vec<_>>(),
            vec![
                Ok(ProcedureRecord::new(
                    ProcedureOperation::Call,
                    Stack::new(vec![Some("baz".into())]),
                    0
                )),
                Ok(ProcedureRecord::new(
                    ProcedureOperation::Return,
                    Stack::new(vec![Some("foo".into()), Some("bar".into())]),
                    42
                )),
                Ok(ProcedureRecord::new(
                    ProcedureOperation::ReturnCall,
                    Stack::new(vec![None; 3]),
                    2045
                ))
            ]
        );
        // spell-checker: enable
    }
}
