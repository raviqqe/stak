use crate::{DurationRecord, Error, Stack, StackedRecord};
use alloc::collections::BTreeMap;
use std::io::Write;

/// Calculates a flamegraph.
pub fn calculate_flamegraph(
    records: impl IntoIterator<Item = Result<DurationRecord, Error>>,
    mut writer: impl Write,
) -> Result<(), Error> {
    let mut map = BTreeMap::<Stack, u128>::new();

    for record in records {
        let record = record?;

        if let Some(time) = map.get_mut(record.stack()) {
            *time += record.time();
        } else {
            map.insert(record.stack().clone(), record.time());
        }
    }

    for (stack, time) in map {
        writeln!(writer, "{} {}", stack.display_local("<local>"), time)?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_calculate_flamegraph() {
        let mut buffer = vec![];

        calculate_flamegraph(
            [
                Ok(DurationRecord::new(
                    Stack::new(vec![None, Some("foo".into()), Some("bar".into())]),
                    1,
                )),
                Ok(DurationRecord::new(
                    Stack::new(vec![None, Some("foo".into()), Some("baz".into())]),
                    2,
                )),
                Ok(DurationRecord::new(
                    Stack::new(vec![None, Some("foo".into())]),
                    3,
                )),
                Ok(DurationRecord::new(
                    Stack::new(vec![None, Some("qux".into())]),
                    4,
                )),
                Ok(DurationRecord::new(
                    Stack::new(vec![None, Some("foo".into()), Some("baz".into())]),
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
                <local>;foo 3
                <local>;foo;bar 1
                <local>;foo;baz 44
                <local>;qux 4
                "
            )
        );
    }
}
