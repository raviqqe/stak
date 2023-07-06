use crate::{Instruction, Operand, Program};
use alloc::{string::String, vec, vec::Vec};

pub(crate) const INTEGER_BASE: u64 = i8::MAX as u64 + 1;

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
            Instruction::Apply(operand, tail) => {
                encode_operand(codes, *operand);
                codes.push(if *tail {
                    Instruction::TAIL_APPLY
                } else {
                    Instruction::APPLY
                });
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

    fn encode_and_decode(program: &Program) -> Program {
        decode(&encode(program)).unwrap()
    }

    fn default_symbols() -> Vec<String> {
        vec!["".to_owned()]
    }

    #[test]
    fn encode_nothing() {
        let program = Program::new(default_symbols(), vec![]);

        assert_eq!(encode_and_decode(&program), program);
    }

    #[test]
    fn encode_symbol() {
        let program = Program::new(vec!["foo".into()], vec![]);

        assert_eq!(encode_and_decode(&program), program);
    }

    #[test]
    fn encode_symbols() {
        let program = Program::new(vec!["foo".into(), "bar".into()], vec![]);

        assert_eq!(encode_and_decode(&program), program);
    }

    #[test]
    fn encode_tail_apply_global() {
        let program = Program::new(
            default_symbols(),
            vec![Instruction::Apply(Operand::Global(0), true)],
        );

        assert_eq!(encode_and_decode(&program), program);
    }

    #[test]
    fn encode_tail_apply_local() {
        let program = Program::new(
            default_symbols(),
            vec![Instruction::Apply(Operand::Local(0), true)],
        );

        assert_eq!(encode_and_decode(&program), program);
    }

    #[test]
    fn encode_apply_global() {
        let program = Program::new(
            default_symbols(),
            vec![Instruction::Apply(Operand::Global(0), false)],
        );

        assert_eq!(encode_and_decode(&program), program);
    }

    #[test]
    fn encode_apply_local() {
        let program = Program::new(
            default_symbols(),
            vec![Instruction::Apply(Operand::Local(0), false)],
        );

        assert_eq!(encode_and_decode(&program), program);
    }

    #[test]
    fn encode_set_global() {
        let program = Program::new(
            default_symbols(),
            vec![Instruction::Set(Operand::Global(0))],
        );

        assert_eq!(encode_and_decode(&program), program);
    }

    #[test]
    fn encode_set_local() {
        let program = Program::new(default_symbols(), vec![Instruction::Set(Operand::Local(0))]);

        assert_eq!(encode_and_decode(&program), program);
    }

    #[test]
    fn encode_get_global() {
        let program = Program::new(
            default_symbols(),
            vec![Instruction::Get(Operand::Global(0))],
        );

        assert_eq!(encode_and_decode(&program), program);
    }

    #[test]
    fn encode_get_local() {
        let program = Program::new(default_symbols(), vec![Instruction::Get(Operand::Local(0))]);

        assert_eq!(encode_and_decode(&program), program);
    }

    #[test]
    fn encode_if() {
        let program = Program::new(
            vec![],
            vec![Instruction::If(
                vec![Instruction::Apply(Operand::Global(0), true)],
                vec![Instruction::Apply(Operand::Global(1), true)],
            )],
        );

        assert_eq!(encode_and_decode(&program), program);
    }

    #[test]
    fn encode_if_with_sequences() {
        let program = Program::new(
            vec![],
            vec![Instruction::If(
                vec![
                    Instruction::Get(Operand::Global(0)),
                    Instruction::Apply(Operand::Global(0), true),
                ],
                vec![
                    Instruction::Get(Operand::Global(1)),
                    Instruction::Apply(Operand::Global(1), true),
                ],
            )],
        );

        assert_eq!(encode_and_decode(&program), program);
    }

    #[test]
    fn encode_large_global_index() {
        let program = Program::new(
            default_symbols(),
            vec![Instruction::Set(Operand::Global(42))],
        );

        assert_eq!(encode_and_decode(&program), program);
    }

    #[test]
    fn encode_large_local_index() {
        let program = Program::new(
            default_symbols(),
            vec![Instruction::Set(Operand::Local(42))],
        );

        assert_eq!(encode_and_decode(&program), program);
    }
}
