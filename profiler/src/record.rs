mod duration_record;
mod procedure_operation;
mod procedure_record;
mod stack;
mod stacked_record;

use core::{fmt::Display, str::FromStr};
pub use duration_record::DurationRecord;
pub use procedure_operation::ProcedureOperation;
pub use procedure_record::ProcedureRecord;
pub use stack::Stack;
pub use stacked_record::StackedRecord;

pub trait Record: Display + FromStr {}
