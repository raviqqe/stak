//! Profiling for Stak Scheme.

mod error;
mod flamegraph;
mod parse;
mod record;
mod record_type;

pub use error::Error;
pub use flamegraph::burn_flamegraph;
pub use parse::parse_records;
pub use record::Record;
pub use record_type::RecordType;

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    #[test]
    fn analyze_call() {
        let mut buffer = vec![];

        burn_flamegraph(
            parse_records(
                &indoc!(
                    "
                    call\tfoo;bar;baz\t0
                    return\tfoo;bar;baz\t42
                    "
                )
                .trim(),
            ),
            &mut buffer,
        )
        .unwrap();

        assert_eq!(String::from_utf8(buffer).unwrap(), "baz;bar;foo 42\n");
    }

    #[test]
    fn analyze_nested_calls() {
        let mut buffer = vec![];

        burn_flamegraph(
            parse_records(
                &indoc!(
                    "
                    call\tbaz\t0
                    call\tbar;baz\t1
                    call\tfoo;bar;baz\t2
                    return\tfoo;bar;baz\t42
                    return\tbar;baz\t84
                    return\tbaz\t126
                    "
                )
                .trim(),
            ),
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
