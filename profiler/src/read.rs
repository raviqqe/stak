use crate::{ProcedureRecord, Record};
use core::str::FromStr;
use std::io::BufRead;

pub fn read_records<R: Record>(
    reader: impl BufRead,
) -> impl Iterator<Item = Result<R, <R as FromStr>::Err>> {
    reader.lines().map(|line| line?.parse())
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use std::io::BufReader;

    #[test]
    fn read() {
        assert_eq!(
            read_records::<ProcedureRecord>(BufReader::new(
                indoc!(
                    "
                    call\tbaz\t0
                    return\tfoo;bar\t42
                    return_call\t;;\t2045
                    "
                )
                .as_bytes()
            ))
            .collect(),
            vec![]
        );
    }
}
