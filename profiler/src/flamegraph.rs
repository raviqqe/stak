use crate::{DurationRecord, Error, Stack};
use std::{collections::HashMap, io::Write};

/// Calculates a flamegraph.
pub fn calculate_flamegraph(
    records: impl IntoIterator<Item = Result<DurationRecord, Error>>,
    mut writer: impl Write,
) -> Result<(), Error> {
    let mut map = HashMap::<Stack, u128>::new();

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
    use pretty_assertions::assert_eq;

    #[test]
    fn test_calculate_flamegraph() {
        let mut buffer = vec![];

        calculate_flamegraph(
            [
                Ok(DurationRecord::new(
                    Stack::from(vec![None, Some("foo".into()), Some("bar".into())]),
                    1,
                )),
                Ok(DurationRecord::new(
                    Stack::from(vec![None, Some("foo".into()), Some("baz".into())]),
                    2,
                )),
                Ok(DurationRecord::new(
                    Stack::from(vec![None, Some("foo".into())]),
                    3,
                )),
                Ok(DurationRecord::new(
                    Stack::from(vec![None, Some("qux".into())]),
                    4,
                )),
            ],
            &mut buffer,
        )
        .unwrap();

        assert_eq!(String::from_utf8(buffer).unwrap(), "foo");
    }
}
