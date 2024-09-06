use crate::Record;
use core::error::Error;
use std::io::{self, Write};

/// Writes profile records.
pub fn write_records<R: Record, E: Error + From<io::Error>>(
    records: impl IntoIterator<Item = Result<R, E>>,
    mut writer: impl Write,
) -> Result<(), E> {
    for record in records {
        writeln!(writer, "{}", record?)?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Error, ProcedureOperation, ProcedureRecord, Stack};
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    #[test]
    fn write() {
        let mut buffer = vec![];

        write_records::<_, Error>(
            [
                Ok(ProcedureRecord::new(
                    ProcedureOperation::Call,
                    Stack::new(vec![Some("baz".into())]),
                    0,
                )),
                Ok(ProcedureRecord::new(
                    ProcedureOperation::Return,
                    Stack::new(vec![Some("foo".into()), Some("bar".into())]),
                    42,
                )),
                Ok(ProcedureRecord::new(
                    ProcedureOperation::ReturnCall,
                    Stack::new(vec![None; 3]),
                    2045,
                )),
            ],
            &mut buffer,
        )
        .unwrap();

        assert_eq!(
            buffer,
            indoc!(
                // spell-checker: disable
                b"
                call\tbaz\t0
                return\tfoo;bar\t42
                return_call\t;;\t2045
                "
            )
        );
    }
}
