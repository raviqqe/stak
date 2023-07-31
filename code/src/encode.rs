use crate::{Instruction, Operand, Program, INSTRUCTION_BITS, INTEGER_BASE, SHORT_INTEGER_BASE};
use alloc::{string::String, vec, vec::Vec};

pub fn encode(program: &Program) -> Vec<u8> {
    let mut codes = vec![];

    encode_instructions(&mut codes, program.instructions());
    encode_symbols(&mut codes, program.symbols());

    codes.reverse();

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

fn encode_instructions(codes: &mut Vec<u8>, instructions: &[Instruction]) {
    for instruction in instructions {
        match instruction {
            Instruction::Call(operand, r#return) => {
                encode_instruction(
                    codes,
                    if *r#return {
                        Instruction::RETURN_CALL
                    } else {
                        Instruction::CALL
                    },
                    encode_operand(*operand),
                );
            }
            Instruction::Closure(arity, body) => {
                encode_instruction(codes, Instruction::CLOSURE, *arity);
                encode_instructions(codes, body);
            }
            Instruction::Set(operand) => {
                encode_instruction(codes, Instruction::SET, encode_operand(*operand));
            }
            Instruction::Get(operand) => {
                encode_instruction(codes, Instruction::GET, encode_operand(*operand));
            }
            Instruction::Constant(operand) => {
                encode_instruction(codes, Instruction::CONSTANT, encode_operand(*operand));
            }
            Instruction::If(then, r#else) => {
                encode_instruction(codes, Instruction::IF, Default::default());

                encode_instructions(codes, then);
                encode_instructions(codes, r#else);
            }
        }
    }
}

fn encode_instruction(codes: &mut Vec<u8>, instruction: u8, integer: u64) {
    let integer = encode_integer(codes, integer);

    codes.push((integer << INSTRUCTION_BITS) | instruction)
}

fn encode_operand(operand: Operand) -> u64 {
    match operand {
        Operand::Symbol(number) => number << 1,
        Operand::Integer(number) => (number << 1) + 1,
    }
}

fn encode_integer(codes: &mut Vec<u8>, integer: u64) -> u8 {
    let mut x = integer / SHORT_INTEGER_BASE;
    let mut bit = 0;

    while x != 0 {
        codes.push(encode_integer_part(x, INTEGER_BASE, bit));
        bit = 1;
        x /= INTEGER_BASE;
    }

    encode_integer_part(integer, SHORT_INTEGER_BASE, bit)
}

fn encode_integer_part(integer: u64, base: u64, bit: u64) -> u8 {
    (((integer % base) << 1) | bit) as u8
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
            vec![Instruction::Call(Operand::Symbol(0), true)],
        ));
    }

    #[test]
    fn encode_return_call_local() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::Call(Operand::Integer(0), true)],
        ));
    }

    #[test]
    fn encode_call_global() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::Call(Operand::Symbol(0), false)],
        ));
    }

    #[test]
    fn encode_call_local() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::Call(Operand::Integer(0), false)],
        ));
    }

    #[test]
    fn encode_close() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::Closure(
                42,
                vec![Instruction::Call(Operand::Integer(0), true)],
            )],
        ));
    }

    #[test]
    fn encode_set_global() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::Set(Operand::Symbol(0))],
        ));
    }

    #[test]
    fn encode_set_local() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::Set(Operand::Integer(0))],
        ));
    }

    #[test]
    fn encode_get_global() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::Get(Operand::Symbol(0))],
        ));
    }

    #[test]
    fn encode_get_global_with_large_index() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::Get(Operand::Symbol(4))],
        ));
    }

    #[test]
    fn encode_get_global_with_very_large_index() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::Get(Operand::Symbol(1000))],
        ));
    }

    #[test]
    fn encode_get_local() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::Get(Operand::Integer(0))],
        ));
    }

    #[test]
    fn encode_if() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::If(
                vec![Instruction::Call(Operand::Symbol(0), true)],
                vec![Instruction::Call(Operand::Symbol(1), true)],
            )],
        ));
    }

    #[test]
    fn encode_if_with_sequences() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::If(
                vec![
                    Instruction::Get(Operand::Symbol(0)),
                    Instruction::Call(Operand::Symbol(0), true),
                ],
                vec![
                    Instruction::Get(Operand::Symbol(1)),
                    Instruction::Call(Operand::Symbol(1), true),
                ],
            )],
        ));
    }

    #[test]
    fn encode_sequence() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![
                Instruction::Get(Operand::Symbol(0)),
                Instruction::Call(Operand::Symbol(0), true),
            ],
        ));
    }

    #[test]
    fn encode_non_zero_global_index() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::Set(Operand::Symbol(42))],
        ));
    }

    #[test]
    fn encode_none_zero_local_index() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::Set(Operand::Integer(42))],
        ));
    }

    #[test]
    fn encode_large_global_index() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::Set(Operand::Symbol(2045))],
        ));
    }

    #[test]
    fn encode_large_local_index() {
        encode_and_decode(&Program::new(
            default_symbols(),
            vec![Instruction::Set(Operand::Integer(2045))],
        ));
    }
}
