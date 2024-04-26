#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Record {}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RecordType {
    Call,
    Return,
    ReturnCall,
}

pub fn parse_records(source: &str) -> impl Iterator<Item = Record> {
    source.lines().map(|line| line.split("\t"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn parse_record() {
        assert_eq!(
            parse_records(
                &indoc!(
                    "
                    call\tfoo;bar;baz\t0
                    return\tfoo;bar;baz\t42
                    "
                )
                .trim()
            )
            .collect::<Vec<_>>(),
            vec![]
        );
    }
}
