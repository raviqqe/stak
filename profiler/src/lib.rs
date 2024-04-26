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
    fn analyze_profile_records() {
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
}
