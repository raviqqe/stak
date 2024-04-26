use crate::{RecordType, COLUMN_SEPARATOR, FRAME_SEPARATOR, LOCAL_PROCEDURE_FRAME};
use stak_vm::{Cons, PrimitiveSet, Profiler, StackSlot, Vm};
use std::{io::Write, time::Instant};

pub struct StackProfiler<T: Write> {
    writer: T,
    start_time: Instant,
}

impl<T: Write> StackProfiler<T> {
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

    fn write_procedure<P: PrimitiveSet>(&mut self, vm: &Vm<P>, code: Cons) {
        let operand = vm.car(code);

        if let Some(symbol) = operand.to_cons() {
            let mut string = vm.car_value(vm.car(symbol)).assume_cons();

            while string != vm.null() {
                write!(
                    self.writer,
                    "{}",
                    char::from_u32(vm.car(string).assume_number().to_i64() as _).unwrap_or('�')
                )
                .unwrap();
                string = vm.cdr(string).assume_cons();
            }
        } else {
            write!(self.writer, "{LOCAL_PROCEDURE_FRAME}").unwrap();
        }
    }

    fn write_stack<P: PrimitiveSet>(&mut self, vm: &Vm<P>) {
        let mut stack = vm.stack();
        let mut first = true;

        while stack != vm.null() {
            stack = if vm.cdr(stack).tag() == StackSlot::Frame as _ {
                if !first {
                    self.write_frame_separator();
                }

                first = false;

                self.write_procedure(vm, vm.car_value(vm.car(stack)).assume_cons());

                vm.cdr_value(vm.car(stack)).assume_cons()
            } else {
                vm.cdr(stack).assume_cons()
            };
        }
    }
}

impl<T: Write, P: PrimitiveSet> Profiler<P> for StackProfiler<T> {
    fn profile_call(&mut self, vm: &Vm<P>, call_code: Cons, r#return: bool) {
        write!(
            self.writer,
            "{}",
            if r#return {
                RecordType::ReturnCall
            } else {
                RecordType::Call
            }
        )
        .unwrap();
        self.write_column_separator();
        self.write_procedure(vm, call_code);
        self.write_frame_separator();
        self.write_stack(vm);
        self.write_column_separator();
        self.write_time();
    }

    fn profile_return(&mut self, vm: &Vm<P>) {
        write!(self.writer, "{}", RecordType::Return).unwrap();
        self.write_column_separator();
        self.write_stack(vm);
        self.write_column_separator();
        self.write_time();
    }
}
