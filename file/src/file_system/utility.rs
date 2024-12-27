use heapless::Vec;
use stak_vm::{Memory, Value};

pub fn decode_path<const N: usize>(memory: &mut Memory, mut list: Value) -> Option<Vec<u8, N>> {
    let mut path = Vec::new();

    while list.assume_cons() != memory.null() {
        path.push(memory.car_value(list).assume_number().to_i64() as u8)
            .ok()?;
        list = memory.cdr_value(list);
    }

    Some(path)
}
