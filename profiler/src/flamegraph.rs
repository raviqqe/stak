use crate::{Error, Record, RecordType};
use std::io::Write;

/// Burns a flamegraph.
pub fn burn_flamegraph<'a>(
    records: impl IntoIterator<Item = Result<Record<'a>, Error>>,
    mut writer: impl Write,
) -> Result<(), Error> {
    let mut stack = vec![];

    for record in records {
        let record = record?;

        match record.r#type() {
            RecordType::Call => {
                stack.push(record);
            }
            RecordType::Return => {
                let previous = stack.pop().ok_or(Error::MissingCallRecord)?;
                writeln!(
                    writer,
                    "{} {}",
                    previous.stack().collect::<Vec<_>>().join(";"),
                    record.time() - previous.time()
                )?;
            }
            RecordType::ReturnCall => {}
        }
    }

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

        burn_flamegraph(
            [
                Ok(Record::new(RecordType::Call, vec!["baz", "bar", "foo"], 0)),
                Ok(Record::new(
                    RecordType::Return,
                    vec!["baz", "bar", "foo"],
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

        burn_flamegraph(
            [
                Ok(Record::new(RecordType::Call, vec!["baz"], 0)),
                Ok(Record::new(RecordType::Call, vec!["baz", "bar"], 1)),
                Ok(Record::new(RecordType::Call, vec!["baz", "bar", "foo"], 2)),
                Ok(Record::new(
                    RecordType::Return,
                    vec!["baz", "bar", "foo"],
                    42,
                )),
                Ok(Record::new(RecordType::Return, vec!["baz", "bar"], 84)),
                Ok(Record::new(RecordType::Return, vec!["baz"], 126)),
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
