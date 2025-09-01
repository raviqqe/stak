use heapless::Vec;
use stak_vm::{Heap, Memory, Value};

pub fn decode_path<const N: usize, H: Heap>(
    memory: &Memory<H>,
    mut list: Value,
) -> Option<Vec<u8, N>> {
    let mut path = Vec::new();

    while list.assume_cons() != memory.null().ok()? {
        path.push(memory.car_value(list).ok()?.assume_number().to_i64() as u8)
            .ok()?;
        list = memory.cdr_value(list).ok()?;
    }

    Some(path)
}
