//! Profiling for Stak Scheme.

mod error;
mod flamegraph;
mod parse;
mod record;
mod record_type;
mod stack_profiler;

pub use error::Error;
pub use flamegraph::calculate_durations;
pub use parse::parse_records;
pub use record::Record;
pub use record_type::RecordType;
pub use stack_profiler::StackProfiler;

const LOCAL_PROCEDURE_FRAME: &str = "_";
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
}
