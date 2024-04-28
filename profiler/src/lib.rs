//! Profiling for Stak Scheme.

extern crate alloc;

mod collapse;
mod duration;
mod error;
mod flamegraph;
mod read;
mod record;
mod stack_profiler;
mod write;

pub use collapse::collapse_stacks;
pub use duration::calculate_durations;
pub use error::Error;
pub use flamegraph::calculate_flamegraph;
pub use read::read_records;
pub use record::{
    DurationRecord, ProcedureOperation, ProcedureRecord, Record, Stack, StackedRecord,
};
pub use stack_profiler::StackProfiler;
pub use write::write_records;

const COLUMN_SEPARATOR: char = '\t';
const FRAME_SEPARATOR: char = ';';

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;
    use std::io::{BufRead, BufReader};

    fn parse_records(reader: impl BufRead) -> impl Iterator<Item = Result<ProcedureRecord, Error>> {
        reader
            .lines()
            .map(|line| -> Result<ProcedureRecord, Error> {
                let mut record = line?.parse::<ProcedureRecord>()?;
                record.stack_mut().reverse_frames();
                Ok(record)
            })
    }

    #[test]
    fn analyze_call() {
        let mut buffer = vec![];

        calculate_durations(
            parse_records(BufReader::new(
                indoc!(
                    "
                    call\tfoo;bar;baz\t0
                    return\tfoo;bar;baz\t42
                    "
                )
                .trim()
                .as_bytes(),
            )),
            &mut buffer,
        )
        .unwrap();

        assert_eq!(String::from_utf8(buffer).unwrap(), "baz;bar;foo\t42\n");
    }

    #[test]
    fn analyze_nested_calls() {
        let mut buffer = vec![];

        calculate_durations(
            parse_records(BufReader::new(
                indoc!(
                    "
                    call\tbaz\t0
                    call\tbar;baz\t1
                    call\tfoo;bar;baz\t2
                    return\tfoo;bar;baz\t42
                    return\tbar;baz\t84
                    return\tbaz\t126
                    "
                )
                .trim()
                .as_bytes(),
            )),
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

    #[test]
    fn analyze_anonymous_procedure_call() {
        let mut buffer = vec![];

        calculate_durations(
            parse_records(BufReader::new(
                indoc!(
                    "
                    call\t;;\t0
                    return\t;;\t42
                    "
                )
                .trim()
                .as_bytes(),
            )),
            &mut buffer,
        )
        .unwrap();

        assert_eq!(String::from_utf8(buffer).unwrap(), ";;\t42\n");
    }
}
