use crate::{Instruction, Operand, Program};
use alloc::{string::String, vec, vec::Vec};

pub const INTEGER_BASE: u64 = i8::MAX as u64 + 1;

pub fn encode(program: &Program) -> Vec<u8> {
    let mut codes = vec![];

    encode_instructions(&mut codes, program.instructions());
    encode_symbols(&mut codes, program.symbols());

    codes
}

fn encode_symbols(codes: &mut Vec<u8>, symbols: &[String]) {
    codes.push(b';');

    for (index, symbol) in symbols.iter().enumerate().rev() {
        for &character in symbol.as_bytes() {
            codes.push(character);
        }

        if index != 0 {
            codes.push(b',');
        }
    }
}

// TODO Use short encodings for instruction operands.
fn encode_instructions(codes: &mut Vec<u8>, instructions: &[Instruction]) {
    for instruction in instructions {
        match instruction {
            Instruction::Call(operand, r#return) => {
                encode_operand(codes, *operand);
                codes.push(if *r#return {
                    Instruction::RETURN_CALL
                } else {
                    Instruction::CALL
                });
            }
            Instruction::Close(arity) => {
                encode_integer(codes, *arity);
                codes.push(Instruction::CLOSE);
            }
            Instruction::Set(operand) => {
                encode_operand(codes, *operand);
                codes.push(Instruction::SET);
            }
            Instruction::Get(operand) => {
                encode_operand(codes, *operand);
                codes.push(Instruction::GET);
            }
            Instruction::Constant(number) => {
                encode_integer(codes, *number);
                codes.push(Instruction::CONSTANT);
            }
            Instruction::If(then, r#else) => {
                codes.push(Instruction::IF);
                encode_instructions(codes, then);
                encode_instructions(codes, r#else);
            }
        }
    }
}

fn encode_operand(codes: &mut Vec<u8>, operand: Operand) {
    match operand {
        Operand::Global(number) => encode_integer(codes, number << 1),

        Operand::Local(number) => encode_integer(codes, (number << 1) + 1),
    }
}

fn encode_integer(codes: &mut Vec<u8>, mut number: u64) {
    let mut rest = false;

    while {
        let part = number % INTEGER_BASE;

        codes.push((if rest { -1 } else { 1 } * part as i64) as u8);

        number /= INTEGER_BASE;
        rest = true;

        number != 0
    } {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::decode;
    use alloc::borrow::ToOwned;

    fn encode_and_decode(program: &Program) {
        assert_eq!(&decode(&encode(program)).unwrap(), program);
    }

    fn default_symbols() -> Vec<String> {
        vec!["".to_owned()]
    }

    #[test]
    fn encode_nothing() {
        encode_and_decode(&Program::new(default_symbols(), vec![]));
    }

    #[test]
    fn encode_symbol() {
        encode_and_decode(&Program::new(vec!["foo".into()], vec![]));
    }

    #[test]
    fn encode_symbols() {
        encode_and_decode(&Program::new(vec!["foo".into(), "bar".into()], vec![]));
    }

    #[test]
    fn encode_return_call_global() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::Call(Operand::Global(0), true)],
        ));
    }

    #[test]
    fn encode_return_call_local() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::Call(Operand::Local(0), true)],
        ));
    }

    #[test]
    fn encode_call_global() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::Call(Operand::Global(0), false)],
        ));
    }

    #[test]
    fn encode_call_local() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::Call(Operand::Local(0), false)],
        ));
    }

    #[test]
    fn encode_set_global() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::Set(Operand::Global(0))],
        ));
    }

    #[test]
    fn encode_set_local() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::Set(Operand::Local(0))],
        ));
    }

    #[test]
    fn encode_get_global() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::Get(Operand::Global(0))],
        ));
    }

    #[test]
    fn encode_get_local() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::Get(Operand::Local(0))],
        ));
    }

    #[test]
    fn encode_if() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::If(
                vec![Instruction::Call(Operand::Global(0), true)],
                vec![Instruction::Call(Operand::Global(1), true)],
            )],
        ));
    }

    #[test]
    fn encode_if_with_sequences() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::If(
                vec![
                    Instruction::Get(Operand::Global(0)),
                    Instruction::Call(Operand::Global(0), true),
                ],
                vec![
                    Instruction::Get(Operand::Global(1)),
                    Instruction::Call(Operand::Global(1), true),
                ],
            )],
        ));
    }

    #[test]
    fn encode_sequence() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![
                Instruction::Get(Operand::Global(0)),
                Instruction::Call(Operand::Global(0), true),
            ],
        ));
    }

    #[test]
    fn encode_large_global_index() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::Set(Operand::Global(42))],
        ));
    }

    #[test]
    fn encode_large_local_index() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::Set(Operand::Local(42))],
        ));
    }
}
