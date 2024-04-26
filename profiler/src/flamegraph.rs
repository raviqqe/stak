use crate::{Error, Record, RecordType};
use std::io::Write;

pub fn burn_flamegraph<'a>(
    records: impl Iterator<Item = Result<Record<'a>, Error>>,
    mut writer: impl Write,
) -> Result<(), Error> {
    let mut stack = vec![];

    for record in records {
        let record = record?;

        match record.r#type() {
            RecordType::Call => {
                stack.push(record);
            }
            RecordType::Return => {
                let previous = stack.pop().ok_or(Error::MissingCallRecord)?;
                write!(
                    writer,
                    "{} {}",
                    previous.stack().collect::<Vec<_>>().join(";"),
                    record.time() - previous.time()
                )?;
            }
            RecordType::ReturnCall => {}
        }

        writeln!(writer)?;
    }

    Ok(())
}
