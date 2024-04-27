use crate::{Error, Record, RecordType, FRAME_SEPARATOR};
use std::io::Write;

/// Calculates durations.
pub fn calculate_durations(
    records: impl IntoIterator<Item = Result<Record, Error>>,
    mut writer: impl Write,
) -> Result<(), Error> {
    let mut first = true;
    let mut stack = vec![];

    for record in records {
        let record = record?;

        if first {
            stack.push(Record::new(RecordType::Call, vec![None], record.time()));
            first = false;
        }

        match record.r#type() {
            RecordType::Call => {
                stack.push(record);
            }
            RecordType::Return => burn_return(&mut stack, &record, &mut writer)?,
            RecordType::ReturnCall => {
                burn_return(&mut stack, &record, &mut writer)?;
                stack.push(record);
            }
        }
    }

    Ok(())
}

fn burn_return(
    stack: &mut Vec<Record>,
    record: &Record,
    mut writer: impl Write,
) -> Result<(), Error> {
    let previous = stack.pop().ok_or(Error::MissingCallRecord)?;

    writeln!(
        writer,
        "{} {}",
        previous
            .stack()
            .map(|frame| frame.unwrap_or_default())
            .collect::<Vec<_>>()
            .join(&FRAME_SEPARATOR.to_string()),
        record.time() - previous.time()
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
                Ok(Record::new(
                    RecordType::Call,
                    vec!["baz".into(), "bar".into(), "foo".into()],
                    0,
                )),
                Ok(Record::new(
                    RecordType::Return,
                    vec![Some("baz".into()), Some("bar".into()), Some("foo".into())],
                    42,
                )),
            ],
            &mut buffer,
        )
        .unwrap();

        assert_eq!(String::from_utf8(buffer).unwrap(), "baz;bar;foo 42\n");
    }

    #[test]
    fn analyze_nested_calls() {
        let mut buffer = vec![];

        calculate_durations(
            [
                Ok(Record::new(RecordType::Call, vec![Some("baz".into())], 0)),
                Ok(Record::new(
                    RecordType::Call,
                    vec![Some("baz".into()), Some("bar".into())],
                    1,
                )),
                Ok(Record::new(
                    RecordType::Call,
                    vec![Some("baz".into()), Some("bar".into()), Some("foo".into())],
                    2,
                )),
                Ok(Record::new(
                    RecordType::Return,
                    vec![Some("baz".into()), Some("bar".into()), Some("foo".into())],
                    42,
                )),
                Ok(Record::new(
                    RecordType::Return,
                    vec![Some("baz".into()), Some("bar".into())],
                    84,
                )),
                Ok(Record::new(RecordType::Return, vec!["baz".into()], 126)),
            ],
            &mut buffer,
        )
        .unwrap();

        assert_eq!(
            String::from_utf8(buffer).unwrap(),
            indoc!(
                "
                baz;bar;foo 40
                baz;bar 83
                baz 126
                "
            )
        );
    }
}
