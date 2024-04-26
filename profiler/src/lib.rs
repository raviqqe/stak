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
