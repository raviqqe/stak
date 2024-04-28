use crate::Record;
use std::io::{self, Write};

pub fn write_records(
    records: impl IntoIterator<Item = impl Record>,
    mut writer: impl Write,
) -> Result<(), io::Error> {
    for record in records {
        writeln!(writer, "{}", record)?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ProcedureOperation, ProcedureRecord, Stack};
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    #[test]
    fn write() {
        let mut buffer = vec![];

        write_records(
            [
                ProcedureRecord::new(
                    ProcedureOperation::Call,
                    Stack::new(vec![Some("baz".into())]),
                    0,
                ),
                ProcedureRecord::new(
                    ProcedureOperation::Return,
                    Stack::new(vec![Some("foo".into()), Some("bar".into())]),
                    42,
                ),
                ProcedureRecord::new(
                    ProcedureOperation::ReturnCall,
                    Stack::new(vec![None; 3]),
                    2045,
                ),
            ],
            &mut buffer,
        )
        .unwrap();

        assert_eq!(
            buffer,
            indoc!(
                b"
                call\tbaz\t0
                return\tfoo;bar\t42
                return_call\t;;\t2045
                "
            )
        );
    }
}
