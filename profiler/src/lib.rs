//! Profiling for Stak Scheme.

mod duration;
mod duration_record;
mod error;
mod flamegraph;
mod parse;
mod procedure_operation;
mod procedure_record;
mod stack;
mod stack_profiler;

pub use duration::calculate_durations;
pub use duration_record::DurationRecord;
pub use error::Error;
pub use flamegraph::calculate_flamegraph;
pub use parse::parse_records;
pub use procedure_operation::ProcedureOperation;
pub use procedure_record::ProcedureRecord;
pub use stack::Stack;
pub use stack_profiler::StackProfiler;

const COLUMN_SEPARATOR: char = '\t';
const FRAME_SEPARATOR: char = ';';

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;
    use std::io::BufReader;

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

        assert_eq!(String::from_utf8(buffer).unwrap(), "baz;bar;foo 42\n");
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
                baz;bar;foo 40
                baz;bar 83
                baz 126
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

        assert_eq!(String::from_utf8(buffer).unwrap(), ";; 42\n");
    }
}
