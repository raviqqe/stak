use stak_vm::{Cons, PrimitiveSet, Profiler, Type, Vm, FRAME_TAG};
use std::{io::Write, time::Instant};

const COLUMN_SEPARATOR: char = '\t';
const FRAME_SEPARATOR: char = ';';

pub struct WriteProfiler<T: Write> {
    writer: T,
    start_time: Instant,
}

impl<T: Write> WriteProfiler<T> {
    pub fn new(writer: T) -> Self {
        Self {
            writer,
            start_time: Instant::now(),
        }
    }

    fn write_type(&mut self, r#return: bool) {
        write!(self.writer, "{}", if r#return { "return" } else { "call" },).unwrap();
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

    fn write_procedure<P: PrimitiveSet>(&mut self, vm: &Vm<P>, code: Cons) -> bool {
        let operand = vm.car(code);

        if let Some(symbol) = operand.to_cons() {
            if vm.car(symbol).tag() == Type::Symbol as _ {
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

                false
            } else {
                true
            }
        } else {
            write!(self.writer, "<local>").unwrap();
            false
        }
    }

    fn write_stack<P: PrimitiveSet>(&mut self, vm: &Vm<P>) {
        let mut stack = vm.stack();

        while stack != vm.null() {
            if vm.cdr(stack).tag() == FRAME_TAG {
                if self.write_procedure(vm, vm.car_value(vm.car(stack)).assume_cons()) {
                    break;
                }

                write!(self.writer, ";").unwrap();

                stack = vm.cdr_value(vm.car(stack)).assume_cons();
            } else {
                stack = vm.cdr(stack).assume_cons();
            }
        }
    }
}

impl<T: Write, P: PrimitiveSet> Profiler<P> for WriteProfiler<T> {
    fn profile_call(&mut self, vm: &Vm<P>, call_code: Cons) {
        self.write_type(false);
        self.write_column_separator();

        if !self.write_procedure(vm, call_code) {
            self.write_frame_separator();
            self.write_stack(vm);
        }

        self.write_column_separator();
        self.write_time();
    }

    fn profile_return(&mut self, vm: &Vm<P>) {
        self.write_type(true);
        self.write_column_separator();
        self.write_stack(vm);
        self.write_column_separator();
        self.write_time();
    }
}
