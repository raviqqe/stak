use crate::{ProcedureOperation, COLUMN_SEPARATOR, FRAME_SEPARATOR};
use stak_vm::{Cons, Memory, Profiler, StackSlot};
use std::{io::Write, time::Instant};

/// A stack profiler.
pub struct StackProfiler<T: Write> {
    writer: T,
    start_time: Instant,
}

impl<T: Write> StackProfiler<T> {
    /// Creates a stack profiler.
    pub fn new(writer: T) -> Self {
        Self {
            writer,
            start_time: Instant::now(),
        }
    }

    fn write_column_separator(&mut self) {
        write!(self.writer, "{COLUMN_SEPARATOR}").unwrap();
    }

    fn write_frame_separator(&mut self) {
        write!(self.writer, "{FRAME_SEPARATOR}").unwrap();
    }

    fn write_time(&mut self) {
        writeln!(
            &mut self.writer,
            "{}",
            Instant::now().duration_since(self.start_time).as_nanos()
        )
        .unwrap();
    }

    fn write_procedure(&mut self, memory: &Memory, code: Cons) {
        let operand = memory.car(code);

        if let Some(symbol) = operand.to_cons() {
            let mut string = memory.car_value(memory.car(symbol)).assume_cons();

            while string != memory.null() {
                write!(
                    self.writer,
                    "{}",
                    char::from_u32(memory.car(string).assume_number().to_i64() as _).unwrap_or('�')
                )
                .unwrap();
                string = memory.cdr(string).assume_cons();
            }
        }
    }

    fn write_stack(&mut self, memory: &Memory) {
        let mut stack = memory.stack();
        let mut first = true;

        while stack != memory.null() {
            stack = if memory.cdr(stack).tag() == StackSlot::Frame as _ {
                if !first {
                    self.write_frame_separator();
                }

                first = false;

                self.write_procedure(memory, memory.car_value(memory.car(stack)).assume_cons());

                memory.cdr_value(memory.car(stack)).assume_cons()
            } else {
                memory.cdr(stack).assume_cons()
            };
        }
    }
}

impl<T: Write> Profiler for StackProfiler<T> {
    fn profile_call(&mut self, memory: &Memory, call_code: Cons, r#return: bool) {
        write!(
            self.writer,
            "{}",
            if r#return {
                ProcedureOperation::ReturnCall
            } else {
                ProcedureOperation::Call
            }
        )
        .unwrap();
        self.write_column_separator();
        self.write_procedure(memory, call_code);
        self.write_frame_separator();
        self.write_stack(memory);
        self.write_column_separator();
        self.write_time();
    }

    fn profile_return(&mut self, memory: &Memory) {
        write!(self.writer, "{}", ProcedureOperation::Return).unwrap();
        self.write_column_separator();
        self.write_stack(memory);
        self.write_column_separator();
        self.write_time();
    }

    fn profile_return(&mut self, memory: &Memory) {
        write!(self.writer, "{}", ProcedureOperation::Return).unwrap();
        self.write_column_separator();
        self.write_stack(memory);
        self.write_column_separator();
        self.write_time();
    }
}
