use crate::{DurationRecord, Error, ProcedureOperation, ProcedureRecord, Stack, StackedRecord};
use std::io::Write;

/// Calculates durations.
pub fn calculate_durations(
    records: impl IntoIterator<Item = Result<ProcedureRecord, Error>>,
    mut writer: impl Write,
) -> Result<(), Error> {
    let mut first = true;
    let mut stack = vec![];

    for record in records {
        let record = record?;

        if first {
            stack.push(ProcedureRecord::new(
                ProcedureOperation::Call,
                Stack::new(vec![None]),
                record.time(),
            ));
            first = false;
        }

        match record.operation() {
            ProcedureOperation::Call => {
                stack.push(record);
            }
            ProcedureOperation::Return => calculate_duration(&mut stack, &record, &mut writer)?,
            ProcedureOperation::ReturnCall => {
                calculate_duration(&mut stack, &record, &mut writer)?;
                stack.push(record);
            }
        }
    }

    Ok(())
}

fn calculate_duration(
    stack: &mut Vec<ProcedureRecord>,
    record: &ProcedureRecord,
    mut writer: impl Write,
) -> Result<(), Error> {
    let previous = stack.pop().ok_or(Error::MissingCallRecord)?;

    writeln!(
        writer,
        "{}",
        DurationRecord::new(previous.stack().clone(), record.time() - previous.time()),
    )?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    #[test]
    fn analyze_call() {
        let mut buffer = vec![];

        calculate_durations(
            [
                Ok(ProcedureRecord::new(
                    ProcedureOperation::Call,
                    Stack::new(vec![
                        Some("baz".into()),
                        Some("bar".into()),
                        Some("foo".into()),
                    ]),
                    0,
                )),
                Ok(ProcedureRecord::new(
                    ProcedureOperation::Return,
                    Stack::new(vec![
                        Some("baz".into()),
                        Some("bar".into()),
                        Some("foo".into()),
                    ]),
                    42,
                )),
            ],
            &mut buffer,
        )
        .unwrap();

        assert_eq!(String::from_utf8(buffer).unwrap(), "baz;bar;foo\t42\n");
    }

    #[test]
    fn analyze_nested_calls() {
        let mut buffer = vec![];

        calculate_durations(
            [
                Ok(ProcedureRecord::new(
                    ProcedureOperation::Call,
                    Stack::new(vec![Some("baz".into())]),
                    0,
                )),
                Ok(ProcedureRecord::new(
                    ProcedureOperation::Call,
                    Stack::new(vec![Some("baz".into()), Some("bar".into())]),
                    1,
                )),
                Ok(ProcedureRecord::new(
                    ProcedureOperation::Call,
                    Stack::new(vec![
                        Some("baz".into()),
                        Some("bar".into()),
                        Some("foo".into()),
                    ]),
                    2,
                )),
                Ok(ProcedureRecord::new(
                    ProcedureOperation::Return,
                    Stack::new(vec![
                        Some("baz".into()),
                        Some("bar".into()),
                        Some("foo".into()),
                    ]),
                    42,
                )),
                Ok(ProcedureRecord::new(
                    ProcedureOperation::Return,
                    Stack::new(vec![Some("baz".into()), Some("bar".into())]),
                    84,
                )),
                Ok(ProcedureRecord::new(
                    ProcedureOperation::Return,
                    Stack::new(vec![Some("baz".into())]),
                    126,
                )),
            ],
            &mut buffer,
        )
        .unwrap();

        assert_eq!(
            String::from_utf8(buffer).unwrap(),
            indoc!(
                "
                baz;bar;foo\t40
                baz;bar\t83
                baz\t126
                "
            )
        );
    }
}
