use anyhow::{bail, Result};
use std::collections::VecDeque;
use std::env;
use synapse::compiler::Compiler;
use synapse::parser::Parser;
use synapse::tokenizer::{Token, Tokenizer};
use synapse::util::read_file;
use synapse::vm::VM;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    match args.get(1) {
        Some(path) => {
            let src = read_file(path)?;

            let mut tokenizer = Tokenizer::new(&src);
            let mut parser = Parser::default();
            let mut compiler = Compiler::default();

            let Some(mut tokens) = tokenizer
                .by_ref()
                .map(|token| {
                    if token != Token::Error {
                        Some(token)
                    } else {
                        None
                    }
                })
                .collect::<Option<VecDeque<Token<'_>>>>()
            else {
                let unrecognized = tokenizer.get_lexer().slice();
                bail!("tokenizer: unexpected token: {}", unrecognized);
            };

            let ast = parser.parse(&mut tokens)?;
            let bytecode = compiler.compile(&ast)?;

            let mut vm = VM::new(bytecode);
            vm.run()?;
        }
        None => bail!("You must pass in a path."),
    }

    Ok(())
}
