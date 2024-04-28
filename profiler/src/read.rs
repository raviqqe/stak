use crate::Record;
use core::str::FromStr;
use std::io::{self, BufRead};

pub fn read_records<R: Record>(
    reader: impl BufRead,
) -> impl Iterator<Item = Result<R, <R as FromStr>::Err>>
where
    <R as FromStr>::Err: From<io::Error>,
{
    reader.lines().map(|line| line?.parse())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ProcedureOperation, ProcedureRecord};
    use indoc::indoc;
    use pretty_assertions::assert_eq;
    use std::io::BufReader;

    #[test]
    fn read() {
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
                ProcedureRecord::new(
                    ProcedureOperation::Call,
                    Stack::new(vec![Some("baz".into())]),
                    0
                ),
                ProcedureRecord::new(
                    ProcedureOperation::Return,
                    Stack::new(vec![Some("baz".into())]),
                    42
                ),
                ProcedureRecord::new(
                    ProcedureOperation::ReturnCall,
                    Stack::new(vec![None; 3]),
                    2045
                )
            ]
        );
    }
}
