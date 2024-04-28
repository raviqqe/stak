use crate::{Error, StackedRecord};

/// Collapses stacks.
pub fn collapse_stacks<R: StackedRecord>(
    records: impl IntoIterator<Item = Result<R, Error>>,
) -> impl Iterator<Item = Result<R, Error>> {
    records.into_iter().map(|record| {
        let mut record = record?;

        record.stack_mut().collapse_frames();

        Ok(record)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{DurationRecord, Stack};
    use pretty_assertions::assert_eq;

    #[test]
    fn collapse() {
        assert_eq!(
            collapse_stacks([
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
            ])
            .collect::<Vec<_>>(),
            vec![
                Ok(DurationRecord::new(
                    Stack::new(vec![Some("bar".into()), Some("foo".into())]),
                    123,
                )),
                Ok(DurationRecord::new(
                    Stack::new(vec![
                        Some("baz".into()),
                        Some("bar".into()),
                        Some("foo".into()),
                    ]),
                    42,
                )),
            ]
        );
    }
}
