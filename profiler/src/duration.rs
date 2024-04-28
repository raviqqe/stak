use crate::{DurationRecord, Error, ProcedureOperation, ProcedureRecord, Stack, StackedRecord};

/// Calculates durations.
pub fn calculate_durations(
    records: impl IntoIterator<Item = Result<ProcedureRecord, Error>>,
) -> impl Iterator<Item = Result<DurationRecord, Error>> {
    let mut stack = vec![];

    records
        .into_iter()
        .enumerate()
        .map(move |(index, record)| {
            let record = record?;

            if index == 0 {
                stack.push(ProcedureRecord::new(
                    ProcedureOperation::Call,
                    Stack::new(vec![None]),
                    record.time(),
                ));
            }

            Ok(match record.operation() {
                ProcedureOperation::Call => {
                    stack.push(record);
                    None
                }
                ProcedureOperation::Return => Some(calculate_duration(&mut stack, &record)?),
                ProcedureOperation::ReturnCall => {
                    let duration = calculate_duration(&mut stack, &record)?;
                    stack.push(record);
                    Some(duration)
                }
            })
        })
        .map(Result::transpose)
        .flatten()
}

fn calculate_duration(
    stack: &mut Vec<ProcedureRecord>,
    record: &ProcedureRecord,
) -> Result<DurationRecord, Error> {
    let previous = stack.pop().ok_or(Error::MissingCallRecord)?;

    Ok(DurationRecord::new(
        previous.stack().clone(),
        record.time() - previous.time(),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn analyze_call() {
        assert_eq!(
            calculate_durations([
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
            ])
            .collect::<Vec<_>>(),
            vec![Ok(DurationRecord::new(
                Stack::new(vec![
                    Some("baz".into()),
                    Some("bar".into()),
                    Some("foo".into()),
                ]),
                0,
            ))]
        );
    }

    #[test]
    fn analyze_nested_calls() {
        assert_eq!(
            calculate_durations([
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
            ])
            .collect::<Vec<_>>(),
            [
                Ok(DurationRecord::new(
                    Stack::new(vec![
                        Some("baz".into()),
                        Some("bar".into()),
                        Some("foo".into()),
                    ]),
                    42,
                )),
                Ok(DurationRecord::new(
                    Stack::new(vec![Some("baz".into()), Some("bar".into())]),
                    84,
                )),
                Ok(DurationRecord::new(
                    Stack::new(vec![Some("baz".into())]),
                    126,
                )),
            ]
        );
    }
}
