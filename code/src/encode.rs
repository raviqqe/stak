use crate::{
    Instruction, Operand, Program, INSTRUCTION_BITS, INTEGER_BASE, SHORT_INTEGER_BASE,
    SYMBOL_SEPARATOR, SYMBOL_TERMINATOR,
};
use alloc::{string::String, vec, vec::Vec};

/// Encodes a program.
pub fn encode(program: &Program) -> Vec<u8> {
    let mut codes = vec![];

    encode_instructions(&mut codes, program.instructions());
    encode_symbols(&mut codes, program.symbols());

    codes.reverse();

    codes
}

fn encode_symbols(codes: &mut Vec<u8>, symbols: &[String]) {
    let empty_count = symbols.iter().fold(
        0,
        |count, symbol| {
            if symbol.is_empty() {
                count + 1
            } else {
                0
            }
        },
    );

    codes.push(SYMBOL_TERMINATOR);

    for (index, symbol) in symbols.iter().enumerate() {
        for &character in symbol.as_bytes() {
            codes.push(character);
        }

        if index + 1 >= symbols.len() - empty_count {
            break;
        }

        codes.push(SYMBOL_SEPARATOR);
    }

    encode_integer(codes, empty_count as u64);
}

fn encode_instructions(codes: &mut Vec<u8>, instructions: &[Instruction]) {
    for (index, instruction) in instructions.iter().enumerate() {
        let r#return = index == instructions.len() - 1;

        match instruction {
            Instruction::Call(count, operand) => {
                encode_integer(codes, encode_operand(*operand));
                encode_instruction(codes, Instruction::CALL, *count, r#return)
            }
            Instruction::Set(operand) => {
                encode_instruction(codes, Instruction::SET, encode_operand(*operand), r#return)
            }
            Instruction::Get(operand) => {
                encode_instruction(codes, Instruction::GET, encode_operand(*operand), r#return)
            }
            Instruction::Constant(operand) => {
                encode_instruction(
                    codes,
                    Instruction::CONSTANT,
                    encode_operand(*operand),
                    r#return,
                );
            }
            Instruction::If(then) => {
                encode_instruction(codes, Instruction::IF, Default::default(), false);
                encode_instructions(codes, then);
            }
            Instruction::Close(arity, body) => {
                encode_instruction(codes, Instruction::CLOSE, *arity, r#return);
                encode_instructions(codes, body);
            }
            Instruction::Skip(count) => encode_instruction(codes, Instruction::SKIP, *count, true),
            Instruction::Nop(operand) => {
                encode_instruction(codes, Instruction::NOP, *operand, r#return)
            }
        }
    }
}

fn encode_instruction(codes: &mut Vec<u8>, instruction: u8, integer: u64, r#return: bool) {
    let integer = encode_short_integer(codes, integer);

    codes.push((integer << INSTRUCTION_BITS) | (instruction << 1) | if r#return { 1 } else { 0 })
}

fn encode_operand(operand: Operand) -> u64 {
    match operand {
        Operand::Symbol(number) => number << 1,
        Operand::Integer(number) => (number << 1) + 1,
    }
}

fn encode_integer(codes: &mut Vec<u8>, integer: u64) {
    let byte = encode_integer_with_base(codes, integer, INTEGER_BASE);
    codes.push(byte);
}

fn encode_short_integer(codes: &mut Vec<u8>, integer: u64) -> u8 {
    encode_integer_with_base(codes, integer, SHORT_INTEGER_BASE)
}

fn encode_integer_with_base(codes: &mut Vec<u8>, integer: u64, base: u64) -> u8 {
    let mut x = integer / base;
    let mut bit = 0;

    while x != 0 {
        codes.push(encode_integer_part(x, INTEGER_BASE, bit));
        bit = 1;
        x /= INTEGER_BASE;
    }

    encode_integer_part(integer, base, bit)
}

fn encode_integer_part(integer: u64, base: u64, bit: u64) -> u8 {
    (((integer % base) << 1) | bit) as u8
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::decode;
    use pretty_assertions::assert_eq;

    fn encode_and_decode(program: &Program) {
        assert_eq!(&decode(&encode(program)).unwrap(), program);
    }

    #[test]
    fn encode_nothing() {
        encode_and_decode(&Program::new(vec![], vec![]));
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
    fn encode_symbols_in_correct_order() {
        assert_eq!(
            encode(&Program::new(
                vec!["foo".into(), "bar".into(), "baz".into()],
                vec![]
            )),
            b"\x00zab\xFErab\xFEoof\xFF"
        );
    }

    #[test]
    fn encode_empty_symbol() {
        encode_and_decode(&Program::new(vec!["".into()], vec![]));
    }

    #[test]
    fn encode_empty_symbols() {
        encode_and_decode(&Program::new(vec!["".into(), "".into()], vec![]));
    }

    #[test]
    fn encode_empty_symbols_and_symbol() {
        encode_and_decode(&Program::new(
            vec!["".into(), "".into(), "foo".into()],
            vec![],
        ));
    }

    #[test]
    fn encode_return_call_global() {
        encode_and_decode(&Program::new(
            vec![],
            vec![Instruction::Call(0, Operand::Symbol(0))],
        ));
    }

    #[test]
    fn encode_return_call_local() {
        encode_and_decode(&Program::new(
            vec![],
            vec![Instruction::Call(0, Operand::Integer(0))],
        ));
    }

    #[test]
    fn encode_call_global() {
        encode_and_decode(&Program::new(
            vec![],
            vec![Instruction::Call(0, Operand::Symbol(0))],
        ));
    }

    #[test]
    fn encode_call_local() {
        encode_and_decode(&Program::new(
            vec![],
            vec![Instruction::Call(0, Operand::Integer(0))],
        ));
    }

    #[test]
    fn encode_call_argument_count() {
        encode_and_decode(&Program::new(
            vec![],
            vec![Instruction::Call(42, Operand::Integer(0))],
        ));
    }

    #[test]
    fn encode_closure() {
        encode_and_decode(&Program::new(
            vec![],
            vec![Instruction::Close(
                42,
                vec![Instruction::Call(0, Operand::Integer(0))],
            )],
        ));
    }

    #[test]
    fn encode_instructions_after_closure() {
        encode_and_decode(&Program::new(
            vec![],
            vec![
                Instruction::Close(42, vec![Instruction::Call(0, Operand::Integer(1))]),
                Instruction::Constant(Operand::Integer(2)),
            ],
        ));
    }

    #[test]
    fn encode_instructions_in_closure() {
        encode_and_decode(&Program::new(
            vec![],
            vec![
                Instruction::Constant(Operand::Integer(0)),
                Instruction::Constant(Operand::Integer(1)),
                Instruction::Close(
                    42,
                    vec![
                        Instruction::Constant(Operand::Integer(2)),
                        Instruction::Constant(Operand::Integer(3)),
                    ],
                ),
                Instruction::Constant(Operand::Integer(4)),
                Instruction::Constant(Operand::Integer(5)),
            ],
        ));
    }

    #[test]
    fn encode_tail_if_instruction_in_closure() {
        encode_and_decode(&Program::new(
            vec![],
            vec![
                Instruction::Constant(Operand::Integer(0)),
                Instruction::Constant(Operand::Integer(1)),
                Instruction::Close(
                    42,
                    vec![
                        Instruction::Constant(Operand::Integer(2)),
                        Instruction::If(vec![
                            Instruction::Constant(Operand::Integer(3)),
                            Instruction::Constant(Operand::Integer(4)),
                        ]),
                        Instruction::Constant(Operand::Integer(5)),
                        Instruction::Constant(Operand::Integer(6)),
                    ],
                ),
                Instruction::Constant(Operand::Integer(7)),
                Instruction::Constant(Operand::Integer(8)),
            ],
        ));
    }

    #[test]
    fn encode_non_tail_if_instruction_in_closure() {
        encode_and_decode(&Program::new(
            vec![],
            vec![
                Instruction::Constant(Operand::Integer(0)),
                Instruction::Constant(Operand::Integer(1)),
                Instruction::Close(
                    42,
                    vec![
                        Instruction::Constant(Operand::Integer(2)),
                        Instruction::Constant(Operand::Integer(3)),
                        Instruction::If(vec![
                            Instruction::Constant(Operand::Integer(4)),
                            Instruction::Constant(Operand::Integer(5)),
                        ]),
                        Instruction::Constant(Operand::Integer(6)),
                        Instruction::Constant(Operand::Integer(7)),
                    ],
                ),
                Instruction::Constant(Operand::Integer(8)),
                Instruction::Constant(Operand::Integer(9)),
            ],
        ));
    }

    #[test]
    fn encode_set_global() {
        encode_and_decode(&Program::new(
            vec![],
            vec![Instruction::Set(Operand::Symbol(0))],
        ));
    }

    #[test]
    fn encode_set_local() {
        encode_and_decode(&Program::new(
            vec![],
            vec![Instruction::Set(Operand::Integer(0))],
        ));
    }

    #[test]
    fn encode_get_global() {
        encode_and_decode(&Program::new(
            vec![],
            vec![Instruction::Get(Operand::Symbol(0))],
        ));
    }

    #[test]
    fn encode_get_global_with_large_index() {
        encode_and_decode(&Program::new(
            vec![],
            vec![Instruction::Get(Operand::Symbol(4))],
        ));
    }

    #[test]
    fn encode_get_global_with_very_large_index() {
        encode_and_decode(&Program::new(
            vec![],
            vec![Instruction::Get(Operand::Symbol(1000))],
        ));
    }

    #[test]
    fn encode_get_local() {
        encode_and_decode(&Program::new(
            vec![],
            vec![Instruction::Get(Operand::Integer(0))],
        ));
    }

    #[test]
    fn encode_if() {
        encode_and_decode(&Program::new(
            vec![],
            vec![
                Instruction::If(vec![Instruction::Call(0, Operand::Symbol(0))]),
                Instruction::Call(0, Operand::Symbol(1)),
            ],
        ));
    }

    #[test]
    fn encode_if_with_sequences() {
        encode_and_decode(&Program::new(
            vec![],
            vec![
                Instruction::If(vec![
                    Instruction::Get(Operand::Symbol(0)),
                    Instruction::Call(0, Operand::Symbol(0)),
                ]),
                Instruction::Get(Operand::Symbol(1)),
                Instruction::Call(0, Operand::Symbol(1)),
            ],
        ));
    }

    #[test]
    fn encode_if_terminated_with_non_tail_call() {
        encode_and_decode(&Program::new(
            vec![],
            vec![
                Instruction::If(vec![Instruction::Get(Operand::Symbol(0))]),
                Instruction::Constant(Operand::Integer(1)),
            ],
        ));
    }

    #[test]
    fn encode_if_with_skip_instruction() {
        encode_and_decode(&Program::new(
            vec![],
            vec![
                Instruction::If(vec![
                    Instruction::Constant(Operand::Integer(0)),
                    Instruction::Skip(1),
                ]),
                Instruction::Constant(Operand::Integer(1)),
                Instruction::Call(0, Operand::Symbol(0)),
            ],
        ));
    }

    #[test]
    fn encode_non_tail_if_instruction() {
        encode_and_decode(&Program::new(
            vec![],
            vec![
                Instruction::If(vec![
                    Instruction::Constant(Operand::Integer(0)),
                    Instruction::Constant(Operand::Integer(1)),
                ]),
                Instruction::Constant(Operand::Integer(2)),
                Instruction::Constant(Operand::Integer(3)),
            ],
        ));
    }

    #[test]
    fn encode_nop() {
        encode_and_decode(&Program::new(vec![], vec![Instruction::Nop(42)]));
    }

    #[test]
    fn encode_sequence() {
        encode_and_decode(&Program::new(
            vec![],
            vec![
                Instruction::Get(Operand::Symbol(0)),
                Instruction::Call(0, Operand::Symbol(0)),
            ],
        ));
    }

    #[test]
    fn encode_non_zero_global_index() {
        encode_and_decode(&Program::new(
            vec![],
            vec![Instruction::Set(Operand::Symbol(42))],
        ));
    }

    #[test]
    fn encode_none_zero_local_index() {
        encode_and_decode(&Program::new(
            vec![],
            vec![Instruction::Set(Operand::Integer(42))],
        ));
    }

    #[test]
    fn encode_large_global_index() {
        encode_and_decode(&Program::new(
            vec![],
            vec![Instruction::Set(Operand::Symbol(2045))],
        ));
    }

    #[test]
    fn encode_large_local_index() {
        encode_and_decode(&Program::new(
            vec![],
            vec![Instruction::Set(Operand::Integer(2045))],
        ));
    }
}
