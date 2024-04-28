use crate::{DurationRecord, Error};
use std::io::Write;

/// Collapses stacks.
pub fn collapse_stacks(
    records: impl IntoIterator<Item = Result<DurationRecord, Error>>,
    mut writer: impl Write,
) -> Result<(), Error> {
    for record in records {
        let mut record = record?;

        record.stack_mut().collapse_frames();

        writeln!(writer, "{record}")?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Stack;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    #[test]
    fn analyze_call() {
        let mut buffer = vec![];

        collapse_stacks(
            [
                Ok(DurationRecord::new(
                    Stack::new(vec![Some("bar".into()), Some("foo".into())]),
                    123,
                )),
                Ok(DurationRecord::new(
                    Stack::new(vec![
                        Some("baz".into()),
                        Some("bar".into()),
                        Some("bar".into()),
                        Some("foo".into()),
                    ]),
                    42,
                )),
            ],
            &mut buffer,
        )
        .unwrap();

        assert_eq!(
            String::from_utf8(buffer).unwrap(),
            indoc!(
                "
                bar;foo\t42
                baz;bar;foo\t42
                "
            )
        );
    }
}
